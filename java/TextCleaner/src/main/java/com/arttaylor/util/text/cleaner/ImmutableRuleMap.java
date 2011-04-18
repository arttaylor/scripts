package com.arttaylor.util.text.cleaner;

import java.util.*;
import java.util.regex.Pattern;

/**
 * Created by IntelliJ IDEA.
 * User: reeses
 * Date: 4/11/11
 * Time: 9:02 PM
 * To change this template use File | Settings | File Templates.
 */


public final class ImmutableRuleMap<K extends Pattern, V extends String> extends HashMap<K, V> {

	public static ImmutableRuleMap<Pattern, String> createImmutableRuleMap(final String[] rulePairs) {
		HashMap<Pattern, String> map = new HashMap<Pattern, String>();
		for (String s : rulePairs) {
			String[] keyPair = s.split("=>");
			if (keyPair.length != 2) {
				keyPair = new String[]{keyPair[0], ""};
			}
			map.put(Pattern.compile(keyPair[0], Pattern.CASE_INSENSITIVE), keyPair[1]);
		}
		return new ImmutableRuleMap<Pattern, String>(map);
	}

	public ImmutableRuleMap(final Map<K, V> map) {
		super(map);
	}

	@Override
	public Set<K> keySet() {
		return Collections.unmodifiableSet(super.keySet());
	}

	@Override
	public Collection<V> values() {
		return Collections.unmodifiableSet((Set<? extends V>) super.values());
	}

	@Override
	public Set entrySet() {
		return Collections.unmodifiableSet(super.entrySet());
	}

	@Override
	public V remove(final Object o) {
		throw new IllegalAccessError("Trying to mutate an instance of com.arttaylor.util.text.cleaner.ImmutableRuleMap");
	}

	@Override
	public void putAll(final Map map) {
		throw new IllegalAccessError("Trying to mutate an instance of com.arttaylor.util.text.cleaner.ImmutableRuleMap");
	}

	@Override
	public void clear() {
		throw new IllegalAccessError("Trying to mutate an instance of com.arttaylor.util.text.cleaner.ImmutableRuleMap");
	}

	@Override
	public V put(final K k, final V v) {
		throw new IllegalAccessError("Trying to mutate an instance of com.arttaylor.util.text.cleaner.ImmutableRuleMap");
	}
}
