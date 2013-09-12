package ar.com.datatsunami.bigdata.cobol;

import java.util.Map;

import org.junit.Test;

import static org.junit.Assert.*;

import ar.com.datatsunami.bigdata.cobol.format.DecimalFormat;
import ar.com.datatsunami.bigdata.cobol.format.LongFormat;
import ar.com.datatsunami.bigdata.cobol.linehandler.PositionalLineHandler;

public class ParseDataSetTest {

	// ------------------------------
	// Cobol structure:
	// ------------------------------
	// (...)
	// 03 ITEMID PIC 9(6).
	// 03 CODE PIC X(5).
	// 03 DESCRIPTION PIC X(15).
	// 03 PRICE PIC S9(5)V99. **************
	// 03 INDEX PIC 9(3)V999.
	// (...)
	//
	// ------------------------------
	// Explanation
	// ------------------------------
	// *** (A) ***
	// 03 ITEMID PIC 9(6).
	// - identifier, numeric, 6 positions
	//
	// *** (B) ***
	// 03 CODE PIC X(5).
	// - alphanumeric code, alphanumeric, 5 positions
	//
	// *** (C) ***
	// 03 DESCRIPTION PIC X(15).
	// - full description, alphanumeric, 15 positions
	//
	// *** (D) ***
	// 03 PRICE PIC S9(5)V99.
	// - price, 5 positions (integer part)
	// -- plus 2 positions for decimals, plus 1 position for SIGN
	//
	// *** (E) ***
	// 03 INDEX PIC 9(3)V999.
	// - some percentual value. 3 positios for integer part,
	// -- plus 3 positions for decimal part (without sign)
	//
	// With this structure, each line should have:
	// - (A) -> 6 positions
	// - (B) -> 5 positions
	// - (C) -> 15 positions
	// - (D) -> 5 (integer) + 2 (decimals) + 1 (sign) = 8 positions
	// - (E) -> 3 (integer) + 3 (decimals) = 6 positions
	// TOTAL: 40
	//
	// Each line will have 40 caracters. Ej:
	//
	// 002541PTRYYFilm 8mm x 7mm 0007199+001500 <- positive
	// 002541PTRYYFilm 8mm x 7mm 0007199-001500 <- negative
	// ------=====---------------========------
	// \-A--/\-B-/\------C------/\---D--/\--E-/

	// xx ITEMID PIC 9(6).
	// xx CODE PIC X(5).
	// xx DESCRIPTION PIC X(15).
	// xx PRICE PIC S9(5)V99. **************
	// xx INDEX PIC 9(3)V999.

	final String line1 = "002541PTRYYFilm 8mm x 7mm 0007199+001500";
	final String line2 = "002541PTRYYFilm 8mm x 7mm 0007199-001500";
	final String shortLine = "002541PTRYYFilm 8mm x 7mm 0007199-00150";

	private void addFieldsToCobolDumpParser(CobolDumpParser cp) {
		cp.add(new Field<Long>(6, "ItemID", new LongFormat()));
		cp.add(new Field<String>(5, "Code"));
		cp.add(new Field<String>(15, "Description"));
		cp.add(new Field<Float>(8, "Price", new DecimalFormat(2, true)));
		cp.add(new Field<Float>(6, "Index", new DecimalFormat(3, false)));
	}

	private void parse(CobolDumpParser cp) throws ParserException {
		Map<String, Object> fields = cp.getItemsWithLabels(line1);
		assertEquals(Long.valueOf(2541), fields.get("ItemID"));
		assertEquals("PTRYY", fields.get("Code"));
		assertEquals("Film 8mm x 7mm", fields.get("Description"));
		assertEquals(Float.valueOf((float) 71.99), fields.get("Price"));
		assertEquals(Float.valueOf((float) 1.5), fields.get("Index"));

		// Test negative values
		fields = cp.getItemsWithLabels(line2);
		assertEquals(Long.valueOf(2541), fields.get("ItemID"));
		assertEquals("PTRYY", fields.get("Code"));
		assertEquals("Film 8mm x 7mm", fields.get("Description"));
		assertEquals(Float.valueOf((float) -71.99), fields.get("Price"));
		assertEquals(Float.valueOf((float) 1.5), fields.get("Index"));

		// Test getItemsValues()
		Object[] values = cp.getItemsValues(line1, new String[] { "ItemID", "Price" });
		assertEquals(2, values.length);
		assertEquals(Long.valueOf(2541), values[0]);
		assertEquals(Float.valueOf((float) 71.99), values[1]);
	}

	@Test
	public void parseLineText() throws ParserException {
		CobolDumpParser cp = new CobolDumpParser();
		this.addFieldsToCobolDumpParser(cp);
		this.parse(cp);
	}

	@Test(expected = IllegalArgumentException.class)
	public void parseShortLineText() throws ParserException {
		CobolDumpParser cp = new CobolDumpParser();
		this.addFieldsToCobolDumpParser(cp);
		cp.getItemsWithLabels(shortLine);
	}

	@Test
	public void parseLineTextWithPositionalLineHandler() throws ParserException {
		CobolDumpParser cp = new CobolDumpParser(new PositionalLineHandler());
		this.addFieldsToCobolDumpParser(cp);
		this.parse(cp);
	}

	@Test(expected = IllegalArgumentException.class)
	public void parseShortLineTextWithPositionalLineHandler() throws ParserException {
		CobolDumpParser cp = new CobolDumpParser(new PositionalLineHandler());
		this.addFieldsToCobolDumpParser(cp);
		cp.getItemsValues(shortLine, new String[] { "ItemID", "Price" });
	}

	@Test
	public void comparePerformance() throws ParserException {

		long iterations;
		if (System.getenv().containsKey("ITERATIONS"))
			iterations = Long.parseLong(System.getenv("ITERATIONS"));
		else
			iterations = 100000;

		/*
		 * Default
		 */
		CobolDumpParser cdpDefault = new CobolDumpParser();
		this.addFieldsToCobolDumpParser(cdpDefault);

		final long startDefault = System.currentTimeMillis();
		for (int i = 0; i < iterations; i++)
			cdpDefault.getItemsWithLabels(line1).get("Code");
		final long endDefault = System.currentTimeMillis();

		/*
		 * Positional
		 */
		CobolDumpParser cdpPositional = new CobolDumpParser(new PositionalLineHandler());
		this.addFieldsToCobolDumpParser(cdpPositional);

		final long startPositional = System.currentTimeMillis();
		String[] fieldsNamed = new String[] { "Code" };
		for (int i = 0; i < iterations; i++)
			cdpDefault.getItemsValues(line1, fieldsNamed);
		final long endPositional = System.currentTimeMillis();

		/*
		 * Print results
		 */
		System.out.println("Default: " + ((endDefault - startDefault) / 1000.0) + " secs.");
		System.out.println("Positional: " + ((endPositional - startPositional) / 1000.0) + " secs.");
	}
}
