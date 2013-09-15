package ar.com.datatsunami.bigdata.cobol.linehandler;

import static org.junit.Assert.assertEquals;

import java.io.UnsupportedEncodingException;
import java.util.Map;

import org.apache.hadoop.io.Text;

import ar.com.datatsunami.bigdata.cobol.CobolDumpParser;
import ar.com.datatsunami.bigdata.cobol.Field;
import ar.com.datatsunami.bigdata.cobol.ParserException;
import ar.com.datatsunami.bigdata.cobol.format.DecimalFormat;
import ar.com.datatsunami.bigdata.cobol.format.LongFormat;

public class LineHandlerTestUtils {

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

	final static String line1 = "002541PTRYYFilm 8mm x 7mm 0007199+001500";
	final static String line2 = "002541PTRYYFilm 8mm x 7mm 0007199-001500";
	final static String shortLine = "002541PTRYYFilm 8mm x 7mm 0007199-00150";

	final static Text line1AsText;
	final static Text line2AsText;

	static {
		Text tmp1 = null;
		Text tmp2 = null;

		try {
			tmp1 = new Text(line1.getBytes("UTF-8"));
			tmp2 = new Text(line2.getBytes("UTF-8"));
		} catch (UnsupportedEncodingException e) {
		}

		line1AsText = tmp1;
		line2AsText = tmp2;
	}

	public static void addFieldsToCobolDumpParser(CobolDumpParser cp) {
		cp.add(new Field<Long>(6, "ItemID", new LongFormat()));
		cp.add(new Field<String>(5, "Code"));
		cp.add(new Field<String>(15, "Description"));
		cp.add(new Field<Float>(8, "Price", new DecimalFormat(2, true)));
		cp.add(new Field<Float>(6, "Index", new DecimalFormat(3, false)));
	}

	public static void parse(CobolDumpParser cp) throws ParserException {
		Map<String, Object> fields = cp.getValuesAsMap(line1);
		assertEquals(Long.valueOf(2541), fields.get("ItemID"));
		assertEquals("PTRYY", fields.get("Code"));
		assertEquals("Film 8mm x 7mm", fields.get("Description"));
		assertEquals(Float.valueOf((float) 71.99), fields.get("Price"));
		assertEquals(Float.valueOf((float) 1.5), fields.get("Index"));

		// Test negative values
		fields = cp.getValuesAsMap(line2);
		assertEquals(Long.valueOf(2541), fields.get("ItemID"));
		assertEquals("PTRYY", fields.get("Code"));
		assertEquals("Film 8mm x 7mm", fields.get("Description"));
		assertEquals(Float.valueOf((float) -71.99), fields.get("Price"));
		assertEquals(Float.valueOf((float) 1.5), fields.get("Index"));

		// Test getItemsValues()
		Object[] values = cp.getValues(line1, new String[] { "ItemID", "Price" });
		assertEquals(2, values.length);
		assertEquals(Long.valueOf(2541), values[0]);
		assertEquals(Float.valueOf((float) 71.99), values[1]);
	}

}
