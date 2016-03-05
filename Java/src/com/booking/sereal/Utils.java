package com.booking.sereal;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.lang.ref.WeakReference;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.regex.Pattern;

import javax.tools.Diagnostic;
import javax.tools.DiagnosticCollector;
import javax.tools.JavaCompiler;
import javax.tools.JavaCompiler.CompilationTask;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.ToolProvider;

public class Utils {
	private static final char[] hexDigits = {
		'0', '1', '2', '3', '4', '5', '6', '7',
		'8', '9', 'a', 'b', 'c', 'd', 'e', 'f',
	};

	public static String join(String[] parts, String seperator) {
		return join( Arrays.asList( parts ), seperator );
	}

	public static String join(List<? extends Object> things, String separator) {
		StringBuilder sb = new StringBuilder();
		for(int i = 0; i < things.size(); i++) {
			sb.append( things.get( i ).toString() );
			if( i < things.size() - 1 ) {
				sb.append( separator );
			}
		}
		return sb.toString();
	}

	public static String dump(Object o) {

		String d = dump( o, 0 );
		already_output.clear();
		return d;
	}

	// so we can not overflow our stack with ciruclar refs
	private static Set<Integer> already_output = new HashSet<Integer>();

	@SuppressWarnings({ "rawtypes", "deprecation" })
	private static String dump(Object o, int indent) {

		if( o != null && already_output.contains( System.identityHashCode( o ) ) ) {
			return "@" + System.identityHashCode( o );
		} else if( o != null) {
			already_output.add( System.identityHashCode( o ) );
		}

		String ind = "";
		for(int i = 0; i < indent; i++) {
			ind += "\t";
		}
		if( o == null ) {
			return "(NULL)";
		} else if( o instanceof Map ) {
			StringBuilder sb = new StringBuilder( ind + "Map@" + System.identityHashCode( o )  +" {\n" );
			Map map = (Map) o;
			Object[] array = map.keySet().toArray();
			try {
				Arrays.sort( array );
			} catch (ClassCastException e) {
				// In case where the array can't be sorted (if the keys don't
				// implement Comparable)
			}
			for(Object key : array) {
				sb.append( ind + dump( key, indent + 1 ) );
				sb.append( " => " );
				sb.append( dump( map.get( key ), indent + 1 ) );
				sb.append( "\n" );
			}
			sb.append( ind ).append( "}" );
			return sb.toString();
		} else if( o instanceof List ) {
			List l = (List) o;
			String type = l.isEmpty() ? "" : l.get( 0 ).getClass().getName();
			StringBuilder sb = new StringBuilder( ind + "List<" + type + ">[\n" );
			for(Object li : l) {
				sb.append( dump( li, indent + 1 ) );
				sb.append( "\n" );
			}
			sb.append( ind ).append( "]" );
			return sb.toString();
		} else if( o.getClass().isArray() ) {
			StringBuilder sb = new StringBuilder( ind + "Array@" + System.identityHashCode( o )+" [\n" );
			int length = Array.getLength( o );
			for(int i = 0; i < length; i++) {
				sb.append( dump( Array.get( o, i ), indent + 1 ) );
				sb.append( "\n" );
			}

			sb.append( ind ).append( "]" );
			return sb.toString();
		}
		else if ( o instanceof Pattern) {
			Pattern pat = (Pattern) o;
			return "/" + pat.pattern() + "/"
					+ (((pat.flags() & Pattern.CANON_EQ) > 0) ? "c" : "")
					+ (((pat.flags() & Pattern.CASE_INSENSITIVE) > 0) ? "i" : "")
					+ (((pat.flags() & Pattern.COMMENTS) > 0) ? "x" : "")
					+ (((pat.flags() & Pattern.DOTALL) > 0) ? "s" : "")
					+ (((pat.flags() & Pattern.LITERAL) > 0) ? "q" : "")
					+ (((pat.flags() & Pattern.MULTILINE) > 0) ? "m" : "")
					+ (((pat.flags() & Pattern.UNICODE_CASE) > 0) ? "l" : "")
					+ (((pat.flags() & Pattern.UNIX_LINES) > 0) ? "u" : "")
					+ "@" + System.identityHashCode( o );

		} else if( o instanceof PerlAlias ) {
			return ind + "Alias: " + dump( ((PerlAlias)o).getValue(), indent );
		} else if( o instanceof PerlReference ) {
			return ind + "Perlref@" + System.identityHashCode( o ) +": " + dump( ((PerlReference)o).getValue(), indent);
		} else if( o instanceof WeakReference) {
			return ind + "(weakref@" + System.identityHashCode( o ) + ") " + dump( ((WeakReference)o).get(), 0 );
		} else if( o instanceof PerlObject) {
			PerlObject po = (PerlObject) o;
			return ind + "Object("+(po.isHash()?"hash":(po.isArray()?"array":"reference"))+"):" + po.getName() + "= " + dump( po.getData(), 0 );
		} else {
			// ad system ident hascode (which is normally memory location) so you
			// can see if things point to the same
			return ind + o.getClass().getSimpleName() + "@" + System.identityHashCode( o ) + ": " + o.toString();
		}

	}

	public static String hexStringFromByteArray(byte[] in) {
		return hexStringFromByteArray(in, -1);
	}

	public static String hexStringFromByteArray(byte[] in, int group) {
		StringBuilder out = new StringBuilder(
			2 +
			in.length * 2 +
			(group != -1 ? (in.length / group + 1) : 0)
		);
		out.append("0x");

		int count = 0;
		for (byte b : in) {
			out.append(hexDigits[(b >> 4) & 0xf]);
			out.append(hexDigits[ b       & 0xf]);

			if( group > 0 && (++count == group) ) {
				out.append(' ');
				count = 0;
			}
		}

		return out.toString();
	}

	public static Object bless(String className, Map<String, Object> structure) {
		Compiler c = new Compiler();
		Object o = c.makeClass( className, structure );
		return o;
	}

	public static Object bless(Class<?> c, Map<String, Object> data) {
		Object instance = null;
		try {
			instance = c.newInstance();
			for(Entry<String, Object> entry : data.entrySet()) {
				Field f = c.getDeclaredField( entry.getKey() );
				f.set( instance, entry.getValue() );
			}

		} catch (InstantiationException e) {
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			e.printStackTrace();
		} catch (SecurityException e) {
			e.printStackTrace();
		} catch (NoSuchFieldException e) {
			e.printStackTrace();
		}
		return instance;
	}

	private static class Compiler {

		private static final String root_package = "com.booking.sereal.dynamic";
		/**
		 * Does the required object initialization and compilation.
		 */
		public Object makeClass(String name, Map<String, Object> data) {
			/* Creating dynamic java source code file object */


			String code = "package "+root_package+";\n" + "public class " + name + " {\n ";
			for(Entry<String, Object> entry : data.entrySet()) {
				code += "public "  + entry.getValue().getClass().getCanonicalName() + " " + entry.getKey() + ";\n";
			}
			code += "}";

			System.err.println("CODE: " + code);

			SimpleJavaFileObject fileObject = new DynamicJavaSourceCodeObject( name, code );

			// new DynamicJavaSourceCodeObject
			// ("com.accordess.ca.DynamicCompilationHelloWorld", sourceCode) ;
			JavaFileObject javaFileObjects[] = new JavaFileObject[] { fileObject };

			/* Instantiating the java compiler */
			JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();

			/**
			 * Retrieving the standard file manager from compiler object, which is
			 * used to provide
			 * basic building block for customizing how a compiler reads and writes
			 * to files.
			 *
			 * The same file manager can be reopened for another compiler task.
			 * Thus we reduce the overhead of scanning through file system and jar
			 * files each time
			 */
			StandardJavaFileManager stdFileManager = compiler.getStandardFileManager( null, Locale.getDefault(), null );

			/*
			 * Prepare a list of compilation units (java source code file objects)
			 * to input to compilation task
			 */
			Iterable<? extends JavaFileObject> compilationUnits = Arrays.asList( javaFileObjects );

			/* Prepare any compilation options to be used during compilation */
			// In this example, we are asking the compiler to place the output
			// files under bin folder.
			String[] compileOptions = new String[] { "-d", "bin" };
			Iterable<String> compilationOptionss = Arrays.asList( compileOptions );

			/* Create a diagnostic controller, which holds the compilation problems */
			DiagnosticCollector<JavaFileObject> diagnostics = new DiagnosticCollector<JavaFileObject>();

			/*
			 * Create a compilation task from compiler by passing in the required
			 * input objects prepared above
			 */
			CompilationTask compilerTask = compiler.getTask( null, stdFileManager, diagnostics, compilationOptionss, null, compilationUnits );

			// Perform the compilation by calling the call method on compilerTask
			// object.
			boolean status = compilerTask.call();

			Object instance = null;
			if( !status ) {// If compilation error occurs
				/* Iterate through each compilation problem and print it */
				for(Diagnostic<? extends JavaFileObject> diagnostic : diagnostics.getDiagnostics()) {
					System.out.format( "Error on line %d in %s", diagnostic.getLineNumber(), diagnostic );
				}
			} else {
				// make our object
				try {
					Class<?> c = Class.forName( root_package + "." + name );
					instance = c.newInstance();
					// set its fields
					System.out.println("all");
					for(Field f : c.getDeclaredFields() ) {
						System.out.println("Filed: " + f);
					}
					for(Entry<String, Object> entry : data.entrySet()) {
						System.out.println("Field: " + c.getDeclaredField( entry.getKey() ) );
						Field f =c.getDeclaredField( entry.getKey() );
						f.set( instance, entry.getValue() );
					}

				} catch (ClassNotFoundException e) {
					e.printStackTrace();
				} catch (SecurityException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (NoSuchFieldException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (InstantiationException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (IllegalAccessException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}

			try {
				stdFileManager.close();// Close the file manager
			} catch (IOException e) {
				e.printStackTrace();
			}

			return instance;
		}

		class DynamicJavaSourceCodeObject extends SimpleJavaFileObject {
			private String sourceCode;

			/**
			 * Converts the name to an URI, as that is the format expected by
			 * JavaFileObject
			 *
			 *
			 * @param fully
			 *           qualified name given to the class file
			 * @param code
			 *           the source code string
			 */
			protected DynamicJavaSourceCodeObject(String name, String code) {
				super( URI.create( "string:///" + name.replaceAll( "\\.", "/" ) + Kind.SOURCE.extension ), Kind.SOURCE );
				this.sourceCode = code;
			}

			@Override
			public CharSequence getCharContent(boolean ignoreEncodingErrors) throws IOException {
				return sourceCode;
			}
		}

	}

	public static Object decodeFile(Decoder decoder, File f) throws SerealException, IOException {
		if (decoder.debugTrace) decoder.trace( "Decoding: " + f.getName() );

		if( !f.exists() ) {
			throw new FileNotFoundException( "No such file: " + f.getCanonicalPath() );
		}

		// read everything
		int size = (int) f.length(); // yeah yeah truncate
		if (decoder.debugTrace) decoder.trace( "File size: " + size );
		byte[] buf = new byte[size];
		FileInputStream fi = new FileInputStream( f );
		try {
			fi.read(buf);
		} finally {
			fi.close();
		}
		if (decoder.debugTrace) decoder.trace( "Raw: " + new String( buf ) );

		decoder.setData(buf);
		Object structure = decoder.decode();
		if (decoder.debugTrace) decoder.trace( "Decoded: " + Utils.dump( structure ) );

		return structure;
	}
}
