package com.arttaylor.util.text.cleaner;

/**
 * Created by IntelliJ IDEA.
 * User: reeses
 * Date: 4/12/11
 * Time: 5:37 PM
 * To change this template use File | Settings | File Templates.
 */

import org.junit.Assert;
import org.junit.Test;

public class ParagraphTest {
	@Test
	public void testCreate() {
		Paragraph p = new Paragraph("Around the rocks the rugged rascal ran.");
		Assert.assertNotNull(p);
	}

	@Test
	public void testGetSentence() {
		String value = "Around the rocks the rugged rascal ran.";
		Paragraph p = new Paragraph(value);
		Sentence[] sentences = p.getSentences();
		Assert.assertNotNull(sentences);
		Assert.assertEquals(sentences.length, 1);
		Assert.assertEquals(value, sentences[0].getText());
	}
}
