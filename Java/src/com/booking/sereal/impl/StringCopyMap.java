package com.booking.sereal.impl;

import java.util.Arrays;

public class StringCopyMap {
	public static final long NOT_FOUND = -1;

	private static final String NO_KEY = null;
	String[] keys;
	private long[] values;
	private int size, maxLoad, modulus;

	public StringCopyMap() {
		init(32);
	}

	private void init(int capacity) {
		keys = new String[capacity];
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

	public final long get(String key) {
		int slot = findSlot(key);

		return keys[slot] == NO_KEY ? NOT_FOUND : values[slot];
	}

	public final void put(String key, long value) {
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
		String[] oldKeys = keys;
		long[] oldValues = values;

		init(keys.length * 2);

		for (int i = 0, max = oldKeys.length; i < max; ++i)
			put(oldKeys[i], oldValues[i]);
	}

	private int findSlot(String key) {
		int slot = key == null ? 0 : key.hashCode() & modulus;
		while (keys[slot] != NO_KEY && !keys[slot].equals(key))
			slot = (slot + 1) & modulus;
		return slot;
	}
}
