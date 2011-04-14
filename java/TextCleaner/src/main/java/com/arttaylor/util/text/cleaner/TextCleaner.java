package com.arttaylor.util.text.cleaner;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.Scanner;

/**
 * Created by IntelliJ IDEA.
 * User: reeses
 * Date: 4/11/11
 * Time: 8:49 PM
 * To change this template use File | Settings | File Templates.
 */
public class TextCleaner {

	public static void main(final String[] args) {
		RuleSet rs = new ZinsserRuleSet();
		if (args.length > 0) {
			for (String arg : args) {
				Reader reader;
				try {
					reader = new FileReader(arg);
				} catch (FileNotFoundException f) {
					System.err.println("[Error] File not found: " + arg);
					continue;
				}
				printCleanedInput(rs, reader);
			}
		} else {
			printCleanedInput(rs, new InputStreamReader(System.in));
		}
	}

	private static void printCleanedInput(final RuleSet rs, final Reader reader) {
		Scanner scanner = new Scanner(reader);
		while (scanner.hasNextLine()) {
			String line = scanner.nextLine();
			Paragraph p = new Paragraph(line);
			System.out.print(rs.applyRules(p).getText());
		}
	}
}
