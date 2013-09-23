package ar.com.datatsunami.bigdata.cobol;

import static org.junit.Assert.assertArrayEquals;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Map;

import org.junit.Test;

import ar.com.datatsunami.bigdata.cobol.field.FloatBasedDecimalField;
import ar.com.datatsunami.bigdata.cobol.field.LongField;
import ar.com.datatsunami.bigdata.cobol.field.StringField;

public class SimpleTestFromFile {

	@Test
	public void testGetValuesAsMap() throws URISyntaxException, IOException, ParserException {

		/*
		 * Create the parser
		 */
		CobolDumpParser cp = new CobolDumpParser();

		/*
		 * Populate the fields
		 */
		cp.add(new LongField(6, "ItemID"));
		cp.add(new StringField(5, "Code"));
		cp.add(new StringField(15, "Description"));
		cp.add(new FloatBasedDecimalField(8, "Price", 2, true));
		cp.add(new FloatBasedDecimalField(6, "Index", 3, false));
		cp.freeze();

		/*
		 * Get a line from the file
		 */
		String line = this.getBufferedReader().readLine();

		/*
		 * Parse the line
		 */
		Map<String, Object> map = cp.getValuesAsMap(line);

		/*
		 * Print some values
		 */
		System.out.println(" + The item ID is: " + map.get("ItemID"));
		System.out.println(" + The code is: " + map.get("Code"));
	}

	@Test
	public void testGetValues() throws URISyntaxException, IOException, ParserException {

		/*
		 * Create the parser
		 */
		CobolDumpParser cp = new CobolDumpParser();

		/*
		 * Populate the fields
		 */
		cp.add(new LongField(6, "ItemID"));
		cp.add(new StringField(5, "Code"));
		cp.add(new StringField(15, "Description"));
		cp.add(new FloatBasedDecimalField(8, "Price", 2, true));
		cp.add(new FloatBasedDecimalField(6, "Index", 3, false));
		cp.freeze();

		/*
		 * Get a line from the file
		 */
		String line = this.getBufferedReader().readLine();

		/*
		 * Get the objects for 'ItemID' and 'Price'
		 */
		Object objects[] = cp.getValues(line, "ItemID", "Price");
		Long itemId = (Long) objects[0];
		Float price = (Float) objects[1];

		System.out.println("The price of item #" + itemId + "is $ " + price);
	}

	private BufferedReader getBufferedReader() throws FileNotFoundException, URISyntaxException {
		ClassLoader cl = Thread.currentThread().getContextClassLoader();
		FileReader fs = new FileReader(new File(cl.getResource("cobol-dump.txt").toURI()));
		return new BufferedReader(fs);
	}

	@Test
	public void testGetValuesAsMapAndDump() throws URISyntaxException, IOException, ParserException {

		/*
		 * Create the parser
		 */
		CobolDumpParser cp = new CobolDumpParser();

		/*
		 * Populate the fields
		 */
		cp.add(new LongField(6, "ItemID"));
		cp.add(new StringField(5, "Code"));
		cp.add(new StringField(15, "Description"));
		cp.add(new FloatBasedDecimalField(8, "Price", 2, true));
		cp.add(new FloatBasedDecimalField(6, "Index", 3, false));
		cp.freeze();

		BufferedReader reader = this.getBufferedReader();
		String line = reader.readLine();
		System.out.println(" + line: '" + line + "'");
		Map<String, Object> map = cp.getValuesAsMap(line);

		for (String key : map.keySet())
			System.out.println("   - " + key + ": " + map.get(key));
	}

	@Test
	public void testFile() throws URISyntaxException, IOException, ParserException {

		/*
		 * Create the parser
		 */
		CobolDumpParser cp = new CobolDumpParser();

		/*
		 * Populate the fields
		 */
		cp.add(new LongField(6, "ItemID"));
		cp.add(new StringField(5, "Code"));
		cp.add(new StringField(15, "Description"));
		cp.add(new FloatBasedDecimalField(8, "Price", 2, true));
		cp.add(new FloatBasedDecimalField(6, "Index", 3, false));
		cp.freeze();

		BufferedReader reader = this.getBufferedReader();
		reader.readLine(); // Ignore 1st line

		// 659382,MOUSE,Optical Mouse,0001499+,000000
		assertArrayEquals(new Object[] { 659382l, "MOUSE", "Optical Mouse", 14.99f, 0.0f },
				cp.getValues(reader.readLine(), "ItemID", "Code", "Description", "Price", "Index"));

		// 836482,KBD_X,Usb Keyboard PS0002099+,000000
		assertArrayEquals(new Object[] { 836482l, "KBD_X", "Usb Keyboard PS", 20.99f, 0.0f },
				cp.getValues(reader.readLine(), "ItemID", "Code", "Description", "Price", "Index"));

		// 000000,PROM1,Discount u$s10,0001000-,000000
		assertArrayEquals(new Object[] { 0l, "PROM1", "Discount u$s10", -10.0f, 0.0f },
				cp.getValues(reader.readLine(), "ItemID", "Code", "Description", "Price", "Index"));

		// 000001,PROM2,Discount 10%, 0000000+,010000
		assertArrayEquals(new Object[] { 1l, "PROM2", "Discount 10%", 0.0f, 10.0f },
				cp.getValues(reader.readLine(), "ItemID", "Code", "Description", "Price", "Index"));

	}
}
