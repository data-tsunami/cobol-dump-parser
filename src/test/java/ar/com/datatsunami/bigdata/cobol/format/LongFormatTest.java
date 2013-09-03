package ar.com.datatsunami.bigdata.cobol.format;

import static org.junit.Assert.*;

import org.junit.Test;

public class LongFormatTest {

	@Test
	public void testFormatString() throws InvalidFormatException {
		LongFormat lf = new LongFormat();
		for (String value : new String[] { "1", "1 ", " 1", }) {
			assertEquals("Didn't format value: '" + value + "'", Long.valueOf(1), lf.format(value));
		}

		for (String value : new String[] { "-1", " -1  " }) {
			assertEquals(Long.valueOf(-1), lf.format(value));
		}

		for (String value : new String[] { "", " ", ".", "x1", "x", "1.0", "+1" }) {
			try {
				lf.format(value);
				fail("Format didn't throw exception! Value: '" + value + "'");
			} catch (InvalidFormatException ife) {
			}
		}
	}

	@Test
	public void testCheckEmpty() throws InvalidFormatException {
		LongFormat lf = new LongFormat(Long.valueOf(2));
		assertEquals(lf.format(" "), Long.valueOf(2));
	}

}
