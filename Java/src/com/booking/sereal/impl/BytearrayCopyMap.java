package com.booking.sereal.impl;

import java.util.Arrays;

public class BytearrayCopyMap {
	public static final long NOT_FOUND = -1;

	private static final byte[] NO_KEY = new byte[0];
	byte[][] keys;
	private long[] values;
	private int size, maxLoad, modulus;

	public BytearrayCopyMap() {
		init(32);
	}

	private void init(int capacity) {
		keys = new byte[capacity][];
		values = new long[capacity];
		Arrays.fill(keys, NO_KEY);
		modulus = capacity - 1;
		maxLoad = (int) (capacity * 0.80);
		size = 0;
	}

	public void clear() {
		Arrays.fill(keys, NO_KEY);
		size = 0;
	}

	public final long get(byte[] key) {
		int slot = findSlot(key);

		return keys[slot] == NO_KEY ? NOT_FOUND : values[slot];
	}

	public final void put(byte[] key, long value) {
		int slot = findSlot(key);

		if (keys[slot] == NO_KEY) {
			if (size == maxLoad) {
				rehash();
				slot = findSlot(key);
			}

			keys[slot] = key;
			values[slot] = value;
			size++;
		} else
			values[slot] = value;
	}

	private void rehash() {
		byte[][] oldKeys = keys;
		long[] oldValues = values;

		init(keys.length * 2);

		for (int i = 0, max = oldKeys.length; i < max; ++i)
			put(oldKeys[i], oldValues[i]);
	}

	private int findSlot(byte[] key) {
		int slot = Arrays.hashCode(key) & modulus;
		while (keys[slot] != NO_KEY && !Arrays.equals(keys[slot], key))
			slot = (slot + 1) & modulus;
		return slot;
	}
}
