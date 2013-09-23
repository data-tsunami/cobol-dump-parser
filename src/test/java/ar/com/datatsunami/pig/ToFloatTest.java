package ar.com.datatsunami.pig;

import static org.junit.Assert.assertEquals;

import java.io.IOException;

import org.junit.Test;

public class ToFloatTest {

	@Test
	public void testToFloat() {
		assertEquals(100.01f, Utils.toFloat(100l, 1l, 2l), 0.0001);
		assertEquals(100f, Utils.toFloat(100l, 0l, 2l), 0.0001);
		assertEquals(100f, Utils.toFloat(100l, 1l, 0l), 0.0001);
		assertEquals(null, Utils.toFloat(-100l, 1l, 2l));
		assertEquals(null, Utils.toFloat(100l, -1l, 2l));
		assertEquals(null, Utils.toFloat(100l, 1l, -2l));
	}

	@Test
	public void testGetSign() throws IOException {
		assertEquals(1f, Utils.getSign("+").floatValue(), 0.001d);
		assertEquals(-1f, Utils.getSign("-").floatValue(), 0.001d);
		assertEquals(null, Utils.getSign(""));
		assertEquals(null, Utils.getSign("x"));
	}

}
