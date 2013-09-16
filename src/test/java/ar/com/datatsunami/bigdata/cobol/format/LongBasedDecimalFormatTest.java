package ar.com.datatsunami.bigdata.cobol.format;

import static org.junit.Assert.*;

import org.junit.Test;

public class LongBasedDecimalFormatTest {

	@Test
	public void testFormatString() throws InvalidFormatException {
		LongBasedDecimalFormat lbdf = new LongBasedDecimalFormat(3);
		assertEquals(Long.valueOf(87401239), lbdf.format("87401239+"));
		assertEquals(Long.valueOf(-87401239), lbdf.format("87401239-"));

		LongBasedDecimalFormat lbdf2 = new LongBasedDecimalFormat(3, false);
		assertEquals(Long.valueOf(87401239), lbdf2.format("87401239"));
	}

	@Test(expected = InvalidFormatException.class)
	public void testFormatStringInvalid() throws InvalidFormatException {
		LongBasedDecimalFormat lbdf2 = new LongBasedDecimalFormat(3, false);
		assertEquals(Long.valueOf(87401239), lbdf2.format("87401239+"));
	}

	@Test(expected = InvalidFormatException.class)
	public void testFormatStringInvalid2() throws InvalidFormatException {
		LongBasedDecimalFormat lbdf2 = new LongBasedDecimalFormat(3, false);
		assertEquals(Long.valueOf(87401239), lbdf2.format("87401239-"));
	}

}
