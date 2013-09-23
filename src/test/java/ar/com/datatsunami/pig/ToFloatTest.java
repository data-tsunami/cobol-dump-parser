package ar.com.datatsunami.pig;

import static org.junit.Assert.assertEquals;

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
	public void testGetSign() {
		assertEquals(1f, Utils.getSign("+"), 0.001d);
		assertEquals(-1f, Utils.getSign("-"), 0.001d);
		assertEquals(0f, Utils.getSign(""), 0.001d);
		assertEquals(0f, Utils.getSign("x"), 0.001d);
	}

	// @Test
	// public void testUnsignedDecimalToFloat() throws IOException {
	// UnsignedDecimalToFloat udf = new UnsignedDecimalToFloat();
	// Tuple tuple = TupleFactory.getInstance().newTuple();
	// tuple.append(Long.valueOf(11));
	// tuple.append(Long.valueOf(33));
	// tuple.append(Long.valueOf(2));
	// Float result = udf.exec(tuple);
	// assertEquals(11.33f, result.floatValue(), 0.01f);
	// }

}
