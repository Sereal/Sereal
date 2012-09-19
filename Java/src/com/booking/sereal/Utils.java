package com.booking.sereal;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
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

	/**
	 * Join a list of strings with a separator
	 * 
	 * @param items
	 * @param seperator
	 * @return the joined list or null if it was empty
	 */
	public static String join(List<String> items, final String seperator) {
		return reduce( items, new Combiner<String>() {

			@Override
			public String combine(String t1, String t2) {
				return t2 + seperator + t1;
			}
		} );
	}

	/**
	 * Right reduce a list
	 * 
	 * @param <V>
	 * @param items
	 *           List of V items
	 * @param function
	 *           Combiner that merges to items (like adding)
	 * @return null if items is empty, otherwise the result of right-reducing
	 *         this list using function
	 */
	public static <V> V reduce(List<V> items, Combiner<V> function) {
		if( items.isEmpty() ) {
			return null;
		}
		if( items.size() == 1 ) {
			return items.get( 0 );
		}

		return fold( items.subList( 1, items.size() ), items.get( 0 ), function );
	}

	/**
	 * Right fold a list
	 * 
	 * @param <V>
	 * @param items
	 * @param initial
	 * @param function
	 * @return
	 */
	public static <V> V fold(List<V> items, V initial, Combiner<V> function) {
		if( items.isEmpty() ) {
			return initial;
		}
		if( items.size() == 1 ) {
			return function.combine( items.get( 0 ), initial );
		}

		return fold( items.subList( 1, items.size() ), function.combine( items.get( 0 ), initial ), function );
	}

	/**
	 * The thing Java lacks and I need.
	 * 
	 * @param <IN>
	 *           Type of the elements in the source
	 * @param <OUT>
	 *           Type of the elements in the destination
	 * @param things
	 *           things that go in
	 * @param f
	 *           function that transforms things to other things
	 * @return List of OUT things
	 */
	public static <IN, OUT> List<OUT> map(IN[] things, Function<IN, OUT> f) {
		return map( Arrays.asList( things ), f );
	}

	/**
	 * The thing Java lacks and I need.
	 * 
	 * @param <IN>
	 *           Type of the elements in the source
	 * @param <OUT>
	 *           Type of the elements in the destination
	 * @param things
	 *           things that go in
	 * @param f
	 *           function that transforms things to other things
	 * @return List of OUT things
	 */
	public static <IN, OUT> List<OUT> map(Collection<IN> things, Function<IN, OUT> f) {

		List<OUT> result = new ArrayList<OUT>( things.size() );

		for(IN o : things) {
			result.add( f.apply( o ) );
		}

		return result;
	}

	/**
	 * Combine two items of type T into one.
	 * 
	 * @param <T>
	 */
	public static interface Combiner<T> {
		T combine(T t1, T t2);
	}

	public interface Function<IN, OUT> {

		/**
		 * We don't have function literals which sucks
		 * 
		 * @param o
		 * @return
		 */
		OUT apply(IN o);

	}

	/**
	 * Returns a list of items that match the criteria
	 * 
	 * @param items
	 *           list of items
	 * @param criteria
	 * @return
	 */
	public static <T> List<T> filter(List<T> items, Filter<T, ?> criteria) {
		List<T> out = new ArrayList<T>();
		for(T item : items) {
			if( criteria.accept( item ) ) {
				out.add( item );
			}
		}
		return out;
	}

	/**
	 * Returns a list of items that match all filter criteria
	 * 
	 * @param <T>
	 * @param items
	 * @param filters
	 * @return
	 */
	public static <T> List<T> filter(List<T> items, Collection<Filter<T, ?>> filters) {

		List<T> out = new ArrayList<T>( items );
		for(Filter<T, ?> filter : filters) {
			out.retainAll( filter( out, filter ) );
		}
		return out;
	}

	public static abstract class Filter<T, V> {

		protected V value;

		/**
		 * Creates a new filter filtering on some value
		 * 
		 * @param value
		 */
		public Filter(V value) {
			this.value = value;
		}

		/**
		 * Returns true if item meets some criteria
		 * 
		 * @param item
		 * @return
		 */
		public abstract boolean accept(T item);

		/**
		 * Return the value of this filter
		 * 
		 * @return
		 */
		public V getValue() {
			return value;
		}

		/**
		 * TODO: this is wrong
		 */
		@Override
		public String toString() {
			return "filter" + value;
		}

		@Override
		public boolean equals(Object o) {
			if( o == null || !(o instanceof Filter) ) {
				return false;
			}
			@SuppressWarnings("unchecked")
			Filter<T, V> f = (Filter<T, V>) o;
			return value.equals( f.value );
		}

		@Override
		public int hashCode() {
			return toString().hashCode();
		}
	}

	public static String join(String[] parts, String seperator) {
		return join( Arrays.asList( parts ), seperator );
	}

	/**
	 * Grep a collection of things and produce a new collection of things that
	 * match the criteria.
	 * 
	 * Modifying elements in the resulting list will modify those in the original
	 * list.
	 * 
	 * @param <T>
	 * @param items
	 * @param criteria
	 * @return
	 */
	public static <T> Collection<T> grep(Collection<T> items, Function<T, Boolean> criteria) {
		List<T> out = new ArrayList<T>();
		for(T item : items) {
			if( criteria.apply( item ) ) {
				out.add( item );
			}
		}
		return out;
	}

	public static String join(String separator, List<? extends Object> things) {
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

		return dump( o, 0 );
	}

	@SuppressWarnings({ "rawtypes", "deprecation" })
	private static String dump(Object o, int indent) {

		String ind = "";
		for(int i = 0; i < indent; i++) {
			ind += "\t";
		}
		if( o == null ) {
			return "(NULL)";
		} else if( o instanceof Calendar ) {
			return ((Calendar) o).getTime().toGMTString(); // deprecated but
																			// easiest to see an
																			// actual time :)
		} else if( o instanceof Map ) {
			StringBuilder sb = new StringBuilder( ind + "{\n" );
			Map map = (Map) o;
			Object[] array = map.keySet().toArray();
			try {
				Arrays.sort( array );
			} catch (ClassCastException e) {
				// In case where the array can't be sorted (if the keys don't
				// implement Comparable)
			}
			for(Object key : array) {
				sb.append( ind + dump( key ) );
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
		} else if( o instanceof Reader ) {

			StringBuilder sb = new StringBuilder();
			BufferedReader in = new BufferedReader( (Reader) o );
			String line;
			try {
				while( (line = in.readLine()) != null ) {
					sb.append( line );
				}
			} catch (IOException e) {
				sb.append( join( "\n", map( e.getStackTrace(), new Function<StackTraceElement, String>() {

					@Override
					public String apply(StackTraceElement ste) {
						return ste.toString();
					}
				} ) ) );
			}
			return sb.toString();

		} else if( o.getClass().isArray() ) {

			StringBuilder sb = new StringBuilder( ind + "[\n" );
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
					;
			
		} else if( o instanceof Alias ) {
			return "ALIAS: " + dump( ((Alias)o).value );
		} else if( o instanceof PerlReference ) {
			return "PERLREF@" + System.identityHashCode( o ) +": " + dump( ((PerlReference)o).value );
		} else {
			// ad system ident hascode (which is normally memory location) so you
			// can see if things point to the same
			return ind + o.getClass().getSimpleName() + "@" + System.identityHashCode( o ) + ": " + o.toString();
		}

	}

	public static String hexStringFromByteArray(byte[] in) {
		String out = "";

		for(byte b : in) {
			// System.out.println(b + " -> " + (b & 0xFF));
			out += ((b & 0xFF) < 16 ? "0" : "") + Integer.toHexString( b & 0xFF );

		}

		return "0x" + out;
	}

	public static String binStringFromByteArray(byte[] in) {
		String out = "";

		for(byte b : in) {
			// System.out.println(b + " -> " + (b & 0xFF));
			String bin = Integer.toBinaryString( b & 0xFF ); // xor with 0xff to
																				// force int
																				// promotion,
																				// bytes are singed
			while( bin.length() < 8 ) {
				bin = "0" + bin;
			}
			out += bin;

		}

		return "0b" + out;
	}

	public static Object bless(String className, Map<String, Object> structure) {
		Compiler c = new Compiler();
		Object o = c.makeClass( className, structure );
		return o;
	}

	public static Object bless(Class c, Map<String, Object> data) {
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
				for(Diagnostic diagnostic : diagnostics.getDiagnostics()) {
					System.out.format( "Error on line %d in %s", diagnostic.getLineNumber(), diagnostic );
				}
			} else {
				// make our object
				try {
					Class c = Class.forName( root_package + "." + name );
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
			private String qualifiedName;
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
				this.qualifiedName = name;
				this.sourceCode = code;
			}

			@Override
			public CharSequence getCharContent(boolean ignoreEncodingErrors) throws IOException {
				return sourceCode;
			}

			public String getQualifiedName() {
				return qualifiedName;
			}

			public void setQualifiedName(String qualifiedName) {
				this.qualifiedName = qualifiedName;
			}

			public String getSourceCode() {
				return sourceCode;
			}

			public void setSourceCode(String sourceCode) {
				this.sourceCode = sourceCode;
			}
		}

	}

}
