package com.arttaylor.util.text.cleaner;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Created by IntelliJ IDEA.
 * User: reeses
 * Date: 4/11/11
 * Time: 8:41 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class RuleSet {
	private ImmutableRuleMap<Pattern, String> ruleMap;

	protected StringBuffer removePunctuationArtifacts(final StringBuffer s) {
		Pattern doubleComma = Pattern.compile(",,");
		Pattern commaPeriod = Pattern.compile(Pattern.quote(",."));

		StringBuffer s1 = new StringBuffer();
		Matcher m = doubleComma.matcher(s);
		while (m.find()) {
			m.appendReplacement(s1, "");
		}
		m.appendTail(s1);

		StringBuffer s2 = new StringBuffer();
		m = commaPeriod.matcher(s1);
		while (m.find()) {
			m.appendReplacement(s2, ".");
		}
		m.appendTail(s2);
		return s2;
	}

	public Sentence applyRules(final Sentence sentence) {
		StringBuffer s = new StringBuffer(sentence.getText());
		List<Integer> charsToDelete = new ArrayList<Integer>();
		Pattern[] patternLocations = new Pattern[sentence.getText().length()];
		for (Pattern key : getRuleMap().keySet()) {
			Matcher m = key.matcher(s);
			while (m.find()) {
				patternLocations[m.start()] = key;
			}
		}
		for (Pattern key : patternLocations) {
			if (null == key) {
				continue;
			}
			StringBuffer sb = new StringBuffer();
			Matcher m = key.matcher(s);
			while (m.find()) {
				String replacementString = getRuleMap().get(key);
				m.appendReplacement(sb, replacementString);
				if (replacementString.equals("")) {
					int i = m.start();
					if (i == 0) {
						i = 1;
					}
					charsToDelete.add(i - 1);
				}
			}
			m.appendTail(sb);
			s = sb;
		}
		SortedSet<Integer> sortedSet = new TreeSet<Integer>(charsToDelete);
		Integer[] charsToDeleteArray = sortedSet.toArray(new Integer[0]);
		for (int i = 0; i < charsToDeleteArray.length; ++i) {
			s.deleteCharAt(charsToDeleteArray[i] - i);
		}
		return new Sentence(removePunctuationArtifacts(s).toString().trim());
	}

	public Paragraph applyRules(final Paragraph paragraph) {
		ArrayList<Sentence> sentences = new ArrayList<Sentence>();
		for (Sentence s : paragraph.getSentences()) {
			sentences.add(applyRules(s));
		}
		return new Paragraph(sentences);
	}

	ImmutableRuleMap<Pattern, String> getRuleMap() {
		return this.ruleMap;
	}

	void setRuleMap(ImmutableRuleMap<Pattern, String> aRuleMap) {
		this.ruleMap = aRuleMap;
	}
}
