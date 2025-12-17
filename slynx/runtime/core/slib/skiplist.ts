class SkipListNode<T> {
	public value: T | null;
	public level: number;
	public forward: (SkipListNode<T> | null)[];

	constructor(value: T, level: number) {
		this.value = value;
		this.level = level;
		this.forward = new Array(level + 1).fill(null);
	}
}

export interface SkipListOptions<T> {
	maxLevel?: number;
	probability?: number;
	compareFn?: (a: T, b: T) => number;
}

export class SkipList<T> {
	private size: number;
	private level: number;

	private maxLevel: number;
	private probability: number;

	private head: SkipListNode<T>;
	private compareFn: (a: T, b: T) => number;

	constructor(options: SkipListOptions<T> = {}) {
		const {
			maxLevel = 16,
			probability = 0.5,
			compareFn = this.defaultCompareFn,
		} = options;

		this.size = 0;
		this.level = 0;

		this.maxLevel = maxLevel;
		this.probability = probability;

		this.head = new SkipListNode<T>(Number.NEGATIVE_INFINITY as T, maxLevel);
		this.compareFn = compareFn;
	}

	/**
	 * Default compare function for comparable types
	 */
	private defaultCompareFn(a: T, b: T) {
		if (a === b) {
			return 0;
		}

		return a < b ? -1 : 1;
	}

	/**
	 * Generate a random level for a new node
	 */

	private randomLevel() {
		let level = 0;

		while (Math.random() < this.probability && level < this.maxLevel - 1) {
			level++;
		}

		return level;
	}

	/**
	 * Search for a value in the skip list
	 * Returns the node if found, null otherwise
	 */
	public search(value: T) {
		let current: SkipListNode<T> | null = this.head;

		// Start from the highest level and work down
		for (let i = this.level; i >= 0; i--) {
			while (
				current?.forward[i] &&
				this.compareFn(current?.forward[i]?.value as T, value) < 0
			) {
				current = current?.forward[i] ?? null;
			}
		}

		current = current?.forward[0] ?? null;

		if (current && this.compareFn(current.value as T, value) === 0) {
			return current;
		}

		return null;
	}

	/**
	 * Insert a value into the skip list
	 * Returns true if inserted, false if already exists
	 */
	public insert(value: T) {
		// Create update array to track nodes at each level
		const update: (SkipListNode<T> | null)[] = new Array(
			this.maxLevel + 1,
		).fill(null);

		let current: SkipListNode<T> | null = this.head;

		// Find the insertion point at each level
		for (let i = this.level; i >= 0; i--) {
			while (
				current?.forward[i] &&
				this.compareFn(current?.forward[i]?.value as T, value) < 0
			) {
				current = current?.forward[i] ?? null;
			}

			update[i] = current;
		}

		current = current?.forward[0] ?? null;

		// If value already exists, return false
		if (current && this.compareFn(current.value as T, value) === 0) {
			return false;
		}

		// Generate random level for new node
		const newLevel = this.randomLevel();

		// If new level is higher than current level, update head pointers
		if (newLevel > this.level) {
			for (let i = this.level + 1; i <= newLevel; i++) {
				update[i] = this.head;
			}

			this.level = newLevel;
		}

		// Create new node
		const newNode = new SkipListNode<T>(value, newLevel);

		// Update forward pointers
		for (let i = 0; i <= newLevel; i++) {
			const next = update[i];

			if (next) {
				newNode.forward[i] = next.forward[i];
				next.forward[i] = newNode;
			}
		}

		this.size++;
		return true;
	}

	/**
	 * Delete a value from the skip list
	 * Returns true if deleted, false if not found
	 */
	public delete(value: T) {
		const update: (SkipListNode<T> | null)[] = new Array(
			this.maxLevel + 1,
		).fill(null);
		let current: SkipListNode<T> | null = this.head;

		// Find the node to delete at each level
		for (let i = this.level; i >= 0; i--) {
			while (
				current?.forward[i] &&
				this.compareFn(current?.forward[i]?.value as T, value) < 0
			) {
				current = current?.forward[i] ?? null;
			}

			update[i] = current;
		}

		current = current?.forward[0] ?? null;

		// If node doesn't exist, return false
		if (!current || this.compareFn(current.value as T, value) !== 0) {
			return false;
		}

		// Update forward pointers to skip the deleted node
		for (let i = 0; i <= this.level; i++) {
			const next = update[i];

			if (next) {
				if (next.forward[i] !== current) {
					continue;
				}

				next.forward[i] = current.forward[i];
			}
		}

		// Clean up empty levels
		while (this.level > 0 && this.head.forward[this.level] === null) {
			this.level--;
		}

		this.size--;
		return true;
	}

	/**
	 * Check if skip list contains a value
	 */
	public contains(value: T) {
		return this.search(value) !== null;
	}

	/**
	 * Clear the skip list
	 */
	public clear() {
		this.size = 0;
		this.level = 0;

		this.head = new SkipListNode(Number.NEGATIVE_INFINITY as T, this.maxLevel);
	}

	/**
	 * Get the minimum value
	 */
	public getMin() {
		const min = this.head.forward[0];

		if (this.size === 0 || !min) {
			return null;
		}

		return min;
	}

	/**
	 * Get the maximum value
	 */
	public getMax() {
		if (this.size === 0) {
			return null;
		}

		let current = this.head;

		for (let i = this.level; i >= 0; i--) {
			while (current.forward[i]) {
				current = current.forward[i] as SkipListNode<T>;
			}
		}

		return current.value;
	}

	/**
	 * Get the height (level) of a specific value
	 */
	public getNodeLevel(value: T) {
		const node = this.search(value);
		return node ? node.level + 1 : 0;
	}

	/**
	 * Check if skip list is empty
	 */
	public isEmpty() {
		return this.size === 0;
	}

	/**
	 * Provides visualization utilities for the Skip List.
	 *
	 * Usage:
	 *  - skipList.visualize()        → ASCII-style linear visualization
	 *  - skipList.visualize.table()  → Tabular visualization using console.table
	 *
	 * Intended strictly for debugging and educational purposes.
	 */
	public get visualize() {
		/**
		 * Prints a classic ASCII visualization of the skip list,
		 * showing each level from top to bottom and the forward
		 * pointers horizontally.
		 */
		const output = () => {
			console.log(
				`Skip List (size: ${this.size}, current max level: ${this.level})`,
			);
			console.log("=".repeat(50));

			// Traverse levels from highest to lowest
			for (let level = this.level; level >= 0; level--) {
				let line = `Level ${level}: -∞`;
				let current = this.head.forward[level];

				// Walk horizontally through the current level
				while (current) {
					line += ` → ${current.value}`;
					current = current.forward[level];
				}

				line += " → null";
				console.log(line);
			}

			console.log("=".repeat(50));
		};

		/**
		 * Prints a tabular visualization of the skip list using console.table.
		 *
		 * Each row represents a level.
		 * Each column represents a node position based on level 0 traversal.
		 *
		 * Empty cells indicate that a node does not exist at that level.
		 */
		output.table = () => {
			if (this.size === 0) {
				console.table([]);
				return;
			}

			/**
			 * Collect all nodes in order using level 0 traversal.
			 * This guarantees consistent horizontal alignment.
			 */
			const nodes: SkipListNode<T>[] = [this.head];
			let current = this.head.forward[0];

			while (current) {
				nodes.push(current);
				current = current.forward[0];
			}

			const table: Record<string, Record<string, unknown>> = {};

			// Build table rows from highest level to base level
			for (let level = this.level; level >= 0; level--) {
				const row: Record<string, unknown> = {};

				for (let index = 0; index < nodes.length; index++) {
					const node = nodes[index];

					// Head sentinel
					if (node === this.head) {
						row[`#${index}`] = "-∞";
						continue;
					}

					// Show value only if the node exists at this level
					row[`#${index}`] = node.forward.length > level ? node.value : "";
				}

				table[`Level ${level}`] = row;
			}

			console.table(table);
		};

		return output;
	}

	/**
	 * Get all values in sorted order
	 */
	public toArray() {
		const result: T[] = [];
		let current = this.head.forward[0];

		while (current) {
			result.push(current.value as T);
			current = current.forward[0];
		}

		return result;
	}
}
