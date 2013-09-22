package ar.com.datatsunami.bigdata.cobol;

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
import ar.com.datatsunami.bigdata.cobol.linehandler.PositionalLineHandler;

public class SimpleTestFromFile {

	@Test
	public void testGetValuesAsMap() throws URISyntaxException, IOException, ParserException {

		/*
		 * Create the parser
		 */
		CobolDumpParser cp = new CobolDumpParser(new PositionalLineHandler());

		/*
		 * Populate the fields
		 */
		cp.add(new LongField(6, "ItemID"));
		cp.add(new StringField(5, "Code"));
		cp.add(new StringField(15, "Description"));
		cp.add(new FloatBasedDecimalField(8, "Price", 2, true));
		cp.add(new FloatBasedDecimalField(6, "Index", 3, false));

		BufferedReader reader = this.getBufferedReader();
		String line = reader.readLine();
		Map<String, Object> map = cp.getValuesAsMap(line);

		System.out.println(" + The item ID is: " + map.get("ItemID"));
		System.out.println(" + The code is: " + map.get("Code"));
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
		CobolDumpParser cp = new CobolDumpParser(new PositionalLineHandler());

		/*
		 * Populate the fields
		 */
		cp.add(new LongField(6, "ItemID"));
		cp.add(new StringField(5, "Code"));
		cp.add(new StringField(15, "Description"));
		cp.add(new FloatBasedDecimalField(8, "Price", 2, true));
		cp.add(new FloatBasedDecimalField(6, "Index", 3, false));

		BufferedReader reader = this.getBufferedReader();
		String line = reader.readLine();
		System.out.println(" + line: '" + line + "'");
		Map<String, Object> map = cp.getValuesAsMap(line);

		for (String key : map.keySet())
			System.out.println("   - " + key + ": " + map.get(key));
	}
}
