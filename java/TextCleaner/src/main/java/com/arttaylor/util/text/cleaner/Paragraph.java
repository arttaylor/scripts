package com.arttaylor.util.text.cleaner;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: reeses
 * Date: 4/11/11
 * Time: 8:50 PM
 * To change this template use File | Settings | File Templates.
 */
public final class Paragraph {
	private final String text;

	public Paragraph(final String newText) {
		text = newText;
	}

	public Paragraph(final List<Sentence> newText) {
		text = Paragraph.join(newText, "  ") + "\n";
	}

	public static String join(final Collection<Sentence> s, final String delimiter) {
		StringBuffer buffer = new StringBuffer();
		Iterator<Sentence> iter = s.iterator();
		while (iter.hasNext()) {
			buffer.append(iter.next().getText());
			if (iter.hasNext()) {
				buffer.append(delimiter);
			}
		}
		return buffer.toString();
	}

	Sentence[] getSentences() {
		List<Sentence> sentences = new ArrayList<Sentence>();
		StringBuilder fragment = new StringBuilder();
		for (char c : getText().toCharArray()) {
			if (c != '\n') {
			fragment.append(c);
			}
			if (c == '.' || c == '?') {
				sentences.add(new Sentence(fragment.toString()));
				fragment.setLength(0);
			}
		}
		return (Sentence[]) sentences.toArray(new Sentence[0]);
	}

	public String getText() {
		return text;
	}

	@Override
	public boolean equals(final Object o) {
		if (o instanceof Paragraph) {
			return ((Paragraph) o).getText().equals(getText());
		} else {
			return false;
		}
	}

	public String toString() {
		return this.getText();
	}
}
