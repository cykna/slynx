///Salloc or Slynx alloc(ator) is a "memory allocator", between quotes because instead of memory it allocates space for elements on the dom.
//It's being highly optimized and inspired by libmalloc, focusing on making the allocation and reuse of memory being as fast as possible, focusing mainly on it
//being O(1) in general, and the worst case being O(log n).
//Differently of other libs that does store things in a tree, Slynx focuses on a Data oriented approach, where the data is stored in a way that allows for fast access and manipulation.
//The reason for so it's because of it's main design that focus on it trying to be a highly optimized frontend library with features you'd see on system programming languages.
//Technically, the reason for prefering a Data oriented approach is because of the cpu caching. The cpu caches the contents that were read/written before going to the ram memory, this then
//focuses on making the data contiguous because we try to use the cpu caching as much as possible which leads us to have better read/write operations. Making these things with pointers would in general
//make it slower due to cache misses and due to the fact that the runtime engine might not be able to optimize it in a way as efficient as it could do by using arrays and buffers

import { BplusNode } from "./btree";

///How much elements are able to be allocated by a single web page. In case a running page.
const MAX_ELEMENT_PER_PAGE = 1 << 16;
///The size of the slice on the elements buffer that will be understood as range for small allocations. In case, 0..16384 is used for allocations of small port.
const SMALL_ALLOCATION_SIZE = 1 << 14;
///How many options of 'small allocations' we got. Since this is 8 possibilites, we got, 1,2,4,8,16,32,64,128
const SMALL_ALLOCATION_OPTION_SIZE = 8;
///The size of a bucket for small allocations. By now, 2048 possibilities. This means that we can do up to 2048/alloc_size() allocations. For example, allocations of 16 elements, we can have
//2048/16 = 128 allocations. So up to 128 elements that have 127 children
const SMALL_BUCKET_SIZE = SMALL_ALLOCATION_SIZE / SMALL_ALLOCATION_OPTION_SIZE;
///Size of u32 needed to represent `SMALL_BUCKET_SIZE` bits but for allocations of 1 element
const ALLOC_1_MASK_SIZE = SMALL_BUCKET_SIZE >> 5;
///Size of u32 needed to represent `SMALL_BUCKET_SIZE` bits but for allocations of 2 elements
const ALLOC_2_MASK_SIZE = SMALL_BUCKET_SIZE >> 6;
///Size of u32 needed to represent `SMALL_BUCKET_SIZE` bits but for allocations of 4 elements
const ALLOC_4_MASK_SIZE = SMALL_BUCKET_SIZE >> 7;
///Size of u32 needed to represent `SMALL_BUCKET_SIZE` bits but for allocations of 8 elements
const ALLOC_8_MASK_SIZE = SMALL_BUCKET_SIZE >> 8;
///Size of u32 needed to represent `SMALL_BUCKET_SIZE` bits but for allocations of 16 elements
const ALLOC_16_MASK_SIZE = SMALL_BUCKET_SIZE >> 9;
///Size of u32 needed to represent `SMALL_BUCKET_SIZE` bits but for allocations of 32 elements
const ALLOC_32_MASK_SIZE = SMALL_BUCKET_SIZE >> 10;
///Size of u32 needed to represent `SMALL_BUCKET_SIZE` bits but for allocations of 64 elements
const ALLOC_64_MASK_SIZE = SMALL_BUCKET_SIZE >> 11;
///Size of u32 needed to represent `SMALL_BUCKET_SIZE` bits but for allocations of 128 elements
const ALLOC_128_MASK_SIZE = SMALL_BUCKET_SIZE >> 12;

const MAX_REQUIRED_U32 = ALLOC_128_MASK_SIZE +
  ALLOC_64_MASK_SIZE +
  ALLOC_32_MASK_SIZE +
  ALLOC_16_MASK_SIZE +
  ALLOC_8_MASK_SIZE +
  ALLOC_4_MASK_SIZE +
  ALLOC_2_MASK_SIZE +
  ALLOC_1_MASK_SIZE +
  1;

interface AllocationObject {
  ///Array of pointer to `elements` inside the pool
  children: Uint16Array;
  ///The max capacity this allocation is holdng
  len: number;
  ///The location on the `elements` this allocation starts at on the indexes array
  ptr: number;
}

export const pool = {
  btree: new BplusNode(),
  element_indexes: new Uint16Array(MAX_ELEMENT_PER_PAGE),
  allocations: [] as AllocationObject[],
  elements: [] as HTMLElement[],
  /**
   * A bitmap buffer tracking free/allocated slots across all small allocation buckets.
   *
   * Uses bits to represent allocation state (1 = allocated, 0 = free) for each bucket slot.
   * The buffer is organized in reverse order by allocation size, with each section sized
   * according to how many u32 values are needed to represent all slots for that bucket:
   *
   * Layout (from index 0):
   * - [0..ALLOC_128_MASK_SIZE): Bucket for 128-element allocations (16 slots, 1 u32)
   * - [ALLOC_128_MASK_SIZE..+ALLOC_64_MASK_SIZE): Bucket for 64-element allocations (32 slots, 1 u32)
   * - [...+ALLOC_32_MASK_SIZE): Bucket for 32-element allocations (64 slots, 2 u32)
   * - [...+ALLOC_16_MASK_SIZE): Bucket for 16-element allocations (128 slots, 4 u32)
   * - [...+ALLOC_8_MASK_SIZE): Bucket for 8-element allocations (256 slots, 8 u32)
   * - [...+ALLOC_4_MASK_SIZE): Bucket for 4-element allocations (512 slots, 16 u32)
   * - [...+ALLOC_2_MASK_SIZE): Bucket for 2-element allocations (1024 slots, 32 u32)
   * - [...+ALLOC_1_MASK_SIZE): Bucket for 1-element allocations (2048 slots, 64 u32)
   *
   * Each bucket can have up to SMALL_BUCKET_SIZE / allocation_size slots.
   * The allocate_bucket function searches this bitmap from right to left (offset - mask_size)
   * to find the first available (0 bit) slot, then marks it as allocated (sets bit to 1).
   */
  freed_metadata: new Uint32Array(MAX_REQUIRED_U32),
  /*
   * The slow alloc ptr is a pointer made specifically for whenllocating on the `BIG_ALLOCATION` are, which, as the name says, it's region dedicated to big allocations. This ptr never reduces, instead it just grows
   * but that doesn't mean it's cause 'segfault' problems, instead, it on freeing content, we can instead reuse that freed location instead of alocatting new memory which would grow towards the end
   */
  slow_alloc_ptr:SMALL_ALLOCATION_SIZE,
};

///Returns the next power of 2 greater or equal to `n`. If `n == 11`, the result is `16`
function power_of_2(n: number): number {
  if (!(n & (n - 1))) return n; //already power of 2
  let i = 1;
  while (i < n) i <<= 1;
  return i;
}

///Returns, from the right to the left, the index where the first `0` bit appears. So 0b10 is 0 because the first bit is 0, 0b1001011 is 2 because from the right to the left the 2 bit is 0
function find_zeroed_bit_position(n: number): number {
  n ^= 0xffff; //0xffff because the limit of the heap is this
  return Math.clz32(n & -n);
}

function raw_malloc(index: number, size: number): AllocationObject {
  return {
    children: pool.element_indexes.subarray(index, index + size),
    ptr: index,
    len: size,
  };
}

///Allocates a new memory bucket of memory with the provided `size`. The `mask_size` is used to iterate over N metadata values to find the avaible position.
//The `offset` is where on the metadata buffer we will start looking at to find the flag we want. And the `buf_offset` is that, on finding, allocates if offseted by it. The formula for so is
//`buf_offset + size * index_of(found_bit)`
function allocate_bucket(
  size: number,
  mask_size: number,
  offset: number,
  buf_offset: number,
): AllocationObject {
  let next_avaible = 0;

  while (mask_size > 0) {
    if (
      (next_avaible =
        find_zeroed_bit_position(pool.freed_metadata[offset - mask_size]) ^
        31) >=
        0
    ) {
      pool.freed_metadata[offset - mask_size] |= 1 << next_avaible;
      return raw_malloc(buf_offset + size * next_avaible, size);
    } else mask_size--;
  }
  throw new Error("Out of memory");
}

export function alloc_memory(size:number):AllocationObject{
  const ptr = pool.slow_alloc_ptr;
  if(pool.slow_alloc_ptr + size > 0xffff) throw new Error("Out of memory. Trying to alloc more memory than possible. Actual pointer is "+pool.slow_alloc_ptr.toString(16)+" but trying to allocate "+size.toString(16)+" elements");
  const out = raw_malloc(ptr, size);
  pool.slow_alloc_ptr += size;
  return out;
}

/**
  Salloc of `slynx alloc` allocates an amount of `size` elements to be able to be used. This is rounded to the nearest power of 2 number >= size.
  When the provided size is <=128, bitmasks are used and the pointer of the elements will be allocated on the `SMALL_BUCKETS` session which is O(1).
  The `SMALL_BUCKETS` session is separated by 8 buckets, each containing it's own size, starting from 128 and going to 1. When allocating 128, you allocate on the
  small bucket an allocation that can handle up to 128 elements, since the limit per bucket is `SMALL_BUCKETS` elements,
*/
export function salloc(size: number): AllocationObject {
  if (size === 0) throw new Error("Cannot make an allocation of 0 elements");
  if (size > 128) {
    const ptr = pool.btree.remove(size);
    if (ptr === -1) return alloc_memory(size);
    else return { ptr, len: size, children: pool.element_indexes.subarray(ptr, ptr + size) };
  }
  switch ((size = Math.min(power_of_2(size), 128))) {
    case 128:
      return allocate_bucket(size, 1, 1, 0);
    case 64:
      return allocate_bucket(size, ALLOC_64_MASK_SIZE, 2, SMALL_BUCKET_SIZE);
    case 32:
      return allocate_bucket(
        size,
        ALLOC_32_MASK_SIZE,
        4,
        SMALL_BUCKET_SIZE * 2,
      );
    case 16:
      return allocate_bucket(
        size,
        ALLOC_16_MASK_SIZE,
        8,
        SMALL_BUCKET_SIZE * 3,
      );
    case 8:
      return allocate_bucket(
        size,
        ALLOC_8_MASK_SIZE,
        16,
        SMALL_BUCKET_SIZE * 4,
      );
    case 4:
      return allocate_bucket(
        size,
        ALLOC_4_MASK_SIZE,
        32,
        SMALL_BUCKET_SIZE * 5,
      );
    case 2:
      return allocate_bucket(
        size,
        ALLOC_2_MASK_SIZE,
        64,
        SMALL_BUCKET_SIZE * 6,
      );
    case 1:
      return allocate_bucket(
        size,
        ALLOC_1_MASK_SIZE,
        128,
        SMALL_BUCKET_SIZE * 7,
      );
      default: throw new Error("Unreacheable code")
  }
 
}

/**
 * Frees the given `alloc` object making it's slot now marked as able to be used. Note that after executing free on the provided allocation, this function doesn't provide any
 * guarantees that, modifications on the allocation after this is called, won't corrupt data or the code will break due to some security check. Since this is programmed to be used
 * on the slynx compiler, this function(as well as salloc) is unsafe was intended to be used when you know what you're doing
 */
export function sfree(alloc: AllocationObject) {
  if(alloc.len > 128) {
    pool.btree.insert(alloc.len, alloc.ptr);
    return 0;
  }
  const bucket_size = Math.clz32(alloc.len) ^ 31; //the number X that is 2^X = alloc.len.
  const bucket_index = 7 - bucket_size;

  const bucket_offset = SMALL_BUCKET_SIZE * bucket_index;
  const bit_idx = (alloc.ptr - bucket_offset) >> bucket_size;

  pool.freed_metadata[1 << bucket_index - 1] &= ~(1 << bit_idx);

  return 0;
}
