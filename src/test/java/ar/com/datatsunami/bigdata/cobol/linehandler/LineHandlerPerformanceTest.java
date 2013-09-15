package ar.com.datatsunami.bigdata.cobol.linehandler;

import org.junit.Test;

import ar.com.datatsunami.bigdata.cobol.CobolDumpParser;
import ar.com.datatsunami.bigdata.cobol.ParserException;

public class LineHandlerPerformanceTest {

	@Test
	public void comparePerformance() throws ParserException {

		// How many iterations have run
		long iterationsRegexLineHandler = 0;
		long iterationsPositionalLineHandler = 0;

		// Hoy many operations each iteration does
		final long iterationSize = 100000;

		// Start time
		long startTime = -1;

		final long msToRun;
		if (System.getenv().containsKey("MS_TO_RUN"))
			msToRun = Long.parseLong(System.getenv("MS_TO_RUN"));
		else
			msToRun = 2000;

		/*
		 * RegexLineHandler
		 */
		System.out.println(" - Checking performance of RegexLineHandler...");
		CobolDumpParser cdpDefault = new CobolDumpParser(new RegexLineHandler());
		LineHandlerTestUtils.addFieldsToCobolDumpParser(cdpDefault);
		final long startRegexLineHandler = System.currentTimeMillis();

		// --- 8< 8< 8< ---
		startTime = System.currentTimeMillis();
		while ((System.currentTimeMillis() - startTime) < msToRun) {
			for (int i = 0; i < iterationSize; i++) {
				cdpDefault.getItemsWithLabels(LineHandlerTestUtils.line1).get("Code");
				iterationsRegexLineHandler++;
			}
		}
		// --- >8 >8 >8 ---
		final long endRegexLineHandler = System.currentTimeMillis();

		/*
		 * PositionalLineHandler
		 */
		System.out.println(" - Checking performance of PositionalLineHandler...");
		CobolDumpParser cdpPositional = new CobolDumpParser(new PositionalLineHandler());
		LineHandlerTestUtils.addFieldsToCobolDumpParser(cdpPositional);
		String[] fieldsNamed = new String[] { "Code" };
		final long startPositionalLineHandler = System.currentTimeMillis();

		// --- 8< 8< 8< ---
		startTime = System.currentTimeMillis();
		while ((System.currentTimeMillis() - startTime) < msToRun) {
			for (int i = 0; i < iterationSize; i++) {
				cdpDefault.getItemsValues(LineHandlerTestUtils.line1, fieldsNamed);
				iterationsPositionalLineHandler++;
			}
		}
		// --- 8< 8< 8< ---
		final long endPositionalLineHandler = System.currentTimeMillis();

		/*
		 * Print results
		 */
		final double iterPerSecRegexLineHandler = iterationsRegexLineHandler
				/ ((endRegexLineHandler - startRegexLineHandler) / 1000.0);
		System.out.println(" - RegexLineHandler: " + iterationsRegexLineHandler + " iters in "
				+ (endRegexLineHandler - startRegexLineHandler) + " ms. -> " + iterPerSecRegexLineHandler
				+ " iter/sec");

		final double iterPerSecPositionalLineHandler = (iterationsPositionalLineHandler / ((endPositionalLineHandler - startPositionalLineHandler) / 1000.0));
		System.out.println(" - PositionalLineHandler: " + iterationsPositionalLineHandler + " iters in "
				+ (endPositionalLineHandler - startPositionalLineHandler) + " ms. -> "
				+ iterPerSecPositionalLineHandler + " iter/sec");

		System.out.format("  + Enhancement: %.2fX\n", iterPerSecPositionalLineHandler
				/ iterPerSecRegexLineHandler);

	}
}
