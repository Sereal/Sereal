package com.booking.sereal;

import java.io.File;
import java.io.IOException;

public class DeSereal {
	/**
	 * @param args
	 * @throws IOException
	 * @throws SerealException
	 */
	public static void main(String[] args) throws IOException, SerealException {
		if( args.length == 0 ) {
			throw new UnsupportedOperationException( "Usage: DeSereal test_data" );
		}

		DecoderOptions decoder_options = new DecoderOptions()
			.perlReferences(true)
			.perlAliases(true)
			.preferLatin1(true);

		Decoder dec = new Decoder( decoder_options );
		final File target = new File( args[0] ).getCanonicalFile(); // to absorb ".." in paths
//		dec.log.setLevel( Level.FINE );
		Object data = Utils.decodeFile( dec, target );
		System.out.println( Utils.dump( data ) );
	}

}
