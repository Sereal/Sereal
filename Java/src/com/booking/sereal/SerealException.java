package com.booking.sereal;

@SuppressWarnings("serial")
public class SerealException extends Exception {
	public SerealException(String msg) {
		super(msg);
	}

	public SerealException(Throwable cause) {
		super(cause);
	}
}
