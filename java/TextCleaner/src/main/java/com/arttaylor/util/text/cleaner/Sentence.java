package com.arttaylor.util.text.cleaner;

/**
 * Created by IntelliJ IDEA.
 * User: reeses
 * Date: 4/11/11
 * Time: 8:54 PM
 * To change this template use File | Settings | File Templates.
 */
public final class Sentence {
	private final String text;

	public Sentence(final String newText) {
		text = newText;
	}


	public String getText() {
		return text;
	}

	public boolean equals(final Object o) {
		if (o instanceof Sentence) {
			return ((Sentence) o).getText().equals(getText());
		} else {
			return false;
		}
	}
}
