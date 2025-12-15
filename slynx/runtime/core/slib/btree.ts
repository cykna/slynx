

const BTREESIZE = 4;

/** -1 == null */
type Pointer = number;
interface SplitResult {
  left: BplusNode,
  right: BplusNode,
  promoted: number
}

export class BplusNode {
  constructor(public keys:number[]=[], public children: BplusNode[]=[], public values: number[][]=[]){}
  
  split():SplitResult{
    
    const mid = this.keys.length >> 1;
    
    const promoted = this.keys[mid];
    const left = new BplusNode(this.keys.splice(0, mid), this.children.splice(0, mid), this.values.splice(0,mid));
    const right = new BplusNode([...this.keys.splice(1)], [...this.children.splice(1)], [...this.values.splice(1)]);
    return {left, right, promoted}  
  }
  
  private find_index(key:number){
    let i = 0;
    while (key > this.keys[i] && i < this.keys.length) i++;
    const flag = i < this.keys.length;
    return (flag * i) + (!flag * -1);
  }
  
  private check_underflow(idx:number){
    const child = this.children[idx];
    const left = this.children[idx - 1];
    const right = this.children[idx+ 1];
    const minKeys = Math.ceil(BTREESIZE * 0.5) - 1;
  
    if (left && left.keys.length > minKeys) {
      child.keys.unshift(left.keys.pop()!);
      child.values.push(left.values.pop()!);
      this.keys[idx - 1] = child.keys[0]; // atualiza separador
      return;
    }
  
    if (right && right.keys.length > minKeys) {
      child.keys.push(right.keys.pop()!);
      child.values.push(right.values.pop()!);
      this.keys[idx] = right.keys[0]; // atualiza separador
      return;
    }
    if (left) {
      left.keys.push(...child.keys);
      left.values.push(...child.values);
      this.keys.splice(idx - 1, 1); // remove separador
      this.children.splice(idx, 1); // remove child
      return;
    }
    if (right) {
      child.keys.push(...right.keys);
      child.values.push(...right.values);
      this.keys.splice(idx, 1); // remove separador
      this.children.splice(idx+ 1, 1); // remove right
      return;
    }
  }
  
  /** Removes a pointer to some freed location with the provided `size` if it exists. Returns -1 if none exists. */
  remove(size:number):Pointer  {
    const i = this.find_index(size);
    if (i === -1) return -1;
    if (this.children.length === 0) {
      const blocks = this.values[i];
      const ptr = blocks.pop()!;
      if (blocks.length === 0) {
        this.keys.splice(i, 1);
        this.values.splice(i, 1);
        this.check_underflow(i);
      }
      return ptr;
    } 
    const block = this.children[i]!.remove(size);
    if (this.children[i].keys.length < Math.ceil(BTREESIZE * 0.5) - 1) this.check_underflow(i);
    return block;
  }
  /** Insets on this btree, the a free allocation of size `size` which starts at `ptr` */
  insert(size: number, ptr:number){
    let idx = 0;
    for(const key of this.keys) {
      if (size < key) break;
      else if (size === key) { this.values[idx].push(ptr); return; }
      else idx++;
    }
    
    if (this.children.length > 0 && idx < this.children.length) {
      this.children[idx].insert(size, ptr);
      return;
    }
    this.keys.splice(idx, 0, size);
    this.values.splice(idx, 0, [ptr]);
    if(this.overflowing()) {
      const split = this.split();
      this.children.splice(idx, 0, split.left, split.right)
    }
  }
  /** Finds the pointers to freed memories with the provided `size` */
  find(alloc_size:number): number[] | undefined {
    let i = 0;
    while (i < this.keys.length && alloc_size > this.keys[i]) i++;
    if (i < this.keys.length && this.keys[i] === alloc_size) return this.values[i]; 
    return this.children[i]?.find(alloc_size);
  }
  overflowing(){
    return this.keys.length > BTREESIZE;
  }
}

