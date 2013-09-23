package ar.com.datatsunami.pig;

import java.io.IOException;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.InputFormat;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.RecordReader;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.TextInputFormat;
import org.apache.pig.LoadFunc;
import org.apache.pig.backend.executionengine.ExecException;
import org.apache.pig.backend.hadoop.executionengine.mapReduceLayer.PigSplit;
import org.apache.pig.data.DataByteArray;
import org.apache.pig.data.Tuple;
import org.apache.pig.data.TupleFactory;

import ar.com.datatsunami.bigdata.cobol.CobolDumpParser;
import ar.com.datatsunami.bigdata.cobol.ParserException;
import ar.com.datatsunami.bigdata.cobol.field.pig.PigSchema;
import ar.com.datatsunami.bigdata.cobol.linehandler.PositionalLineHandler;
import ar.com.datatsunami.pig.FixedWidthLoaderByStaticFunc.Result;

public class CobolDumpLoader extends LoadFunc {

	private final CobolDumpParser cobolDumpParser;
	private final PositionalLineHandler positionalLineHandler;
	private final List<PigSchema> pigSchemas;
	private final int[] fieldIndexes;

	private static final Log logger = LogFactory.getLog(CobolDumpLoader.class);
	private final TupleFactory tupleFactory = TupleFactory.getInstance();

	@SuppressWarnings("rawtypes")
	private RecordReader reader;

	public CobolDumpLoader(String cobolDumpParserFactoryMethod, String fields) {
		logger.debug("Constructor - cobolDumpParserFactoryMethod: '" + cobolDumpParserFactoryMethod
				+ "' - fields: '" + fields + "'");
		Result result = FixedWidthLoaderByStaticFunc.getAsResult(cobolDumpParserFactoryMethod, fields);
		this.cobolDumpParser = result.cobolDumpParser;
		this.positionalLineHandler = result.positionalLineHandler;
		this.pigSchemas = result.pigSchemas;
		this.fieldIndexes = result.fieldIndexes;
		logger.debug(" - cobolDumpParser: " + cobolDumpParser);
		logger.debug(" - positionalLineHandler: " + positionalLineHandler);
		logger.debug(" - pigSchemas: " + pigSchemas);
		logger.debug(" - fieldIndexes: " + StringUtils.join(fieldIndexes, ","));
	}

	@Override
	public void setLocation(String location, Job job) throws IOException {
		FileInputFormat.setInputPaths(job, location);
	}

	@SuppressWarnings("rawtypes")
	@Override
	public InputFormat getInputFormat() throws IOException {
		return new TextInputFormat();
	}

	@SuppressWarnings("rawtypes")
	@Override
	public void prepareToRead(RecordReader reader, PigSplit split) throws IOException {
		this.reader = reader;
	}

	@Override
	public Tuple getNext() throws IOException {
		try {
			if (!reader.nextKeyValue()) { // why?
				return null;
			}
			Text value = (Text) reader.getCurrentValue();
			return this._getNext(value);
		} catch (InterruptedException e) {
			throw new ExecException(e);
		} catch (ParserException e) {
			throw new ExecException(e);
		}

	}

	private Tuple _getNext(Text value) throws ParserException {
		final String line = value.toString();
		final Tuple tuple = this.tupleFactory.newTuple(this.pigSchemas.size());
		final Object parsedValues[] = this.cobolDumpParser.getValues(line, this.fieldIndexes);
		for (Object parsedValue : parsedValues)
			tuple.append(new DataByteArray(parsedValue.toString()));
		return tuple;
	}

}
