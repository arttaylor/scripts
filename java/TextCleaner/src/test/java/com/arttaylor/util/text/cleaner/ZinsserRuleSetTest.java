package com.arttaylor.util.text.cleaner;

import org.junit.Assert;
import org.junit.Test;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

/**
 * Created by IntelliJ IDEA.
 * User: reeses
 * Date: 4/12/11
 * Time: 10:10 PM
 * To change this template use File | Settings | File Templates.
 */
public class ZinsserRuleSetTest {
	@Test
	public void testApplyRules() throws Exception {
		Sentence badSentence = new Sentence("Moses supposes his toeses are roses but Moses actually supposes erroneously.");
		Sentence expectedResultSentence = new Sentence("Moses supposes his toeses are roses but Moses supposes erroneously.");
		ZinsserRuleSet z = new ZinsserRuleSet();
		Sentence cleanedSentence = z.applyRules(badSentence);
		Assert.assertEquals(expectedResultSentence.getText() + " expected, received " + cleanedSentence.getText(), cleanedSentence, expectedResultSentence);
	}

	@Test
	public void testApplyRulesInAside() throws Exception {
		Sentence badSentence = new Sentence("Moses supposes his toeses are roses but Moses, actually, supposes erroneously.");
		Sentence expectedResultSentence = new Sentence("Moses supposes his toeses are roses but Moses supposes erroneously.");
		ZinsserRuleSet z = new ZinsserRuleSet();
		Sentence cleanedSentence = z.applyRules(badSentence);
		Assert.assertEquals(expectedResultSentence.getText() + " expected, received " + cleanedSentence.getText(), cleanedSentence, expectedResultSentence);
	}

	@Test
	public void testApplyRulesInAsideAtEndOfSentence() throws Exception {
		Sentence badSentence = new Sentence("Moses supposes his toeses are roses but Moses supposes erroneously, actually.");
		Sentence expectedResultSentence = new Sentence("Moses supposes his toeses are roses but Moses supposes erroneously.");
		ZinsserRuleSet z = new ZinsserRuleSet();
		Sentence cleanedSentence = z.applyRules(badSentence);
		Assert.assertEquals(expectedResultSentence.getText() + " expected, received " + cleanedSentence.getText(), cleanedSentence, expectedResultSentence);
	}

	@Test
	public void testApplyRulesInParagraphs() throws Exception {
		String badText = "The rain in Spain stays mainly, by definition, on the plain.  Around the rocks the rugged rascal ran.\n" +
				"By definition, Moses supposes his toeses are roses, but Moses actually supposes erroneously, after all is said and done.\n";
		Paragraph[] expectedResultParagraphs = new Paragraph[]{new Paragraph("The rain in Spain stays mainly on the plain.  Around the rocks the rugged rascal ran.\n"),
				new Paragraph("Moses supposes his toeses are roses, but Moses supposes erroneously.\n")};
		ZinsserRuleSet z = new ZinsserRuleSet();
		Scanner scanner = new Scanner(new StringReader(badText));
		List<Paragraph> cleanedParagraphList = new ArrayList<Paragraph>();
		while (scanner.hasNext()) {
			String line = scanner.nextLine();
			Paragraph badParagraph = new Paragraph(line);
			Paragraph cleanedParagraph = z.applyRules(badParagraph);
			cleanedParagraphList.add(cleanedParagraph);
		}
		Paragraph[] cleanedParagraphs = cleanedParagraphList.toArray(new Paragraph[0]);

		Assert.assertArrayEquals(expectedResultParagraphs[0].toString() + expectedResultParagraphs[1].toString() + " expected, but received " + cleanedParagraphs[0].toString() + cleanedParagraphs[1].toString(),  expectedResultParagraphs, cleanedParagraphs);
	}
}
