/**
 * Jackson dataformat adapter for Sereal.
 * <p>
 * The most useful class is {@link com.booking.sereal.jackson.SerealObjectMapper}, which is a convenience
 * subclass of {@link com.fasterxml.jackson.databind.ObjectMapper}.
 * <p>
 * {@link com.booking.sereal.jackson.SerealParser} and {@link com.booking.sereal.jackson.SerealGenerator} are
 * lower-level adapters for the Jackson API, with a streaming interface, and can be constructed through
 * {@link com.booking.sereal.jackson.SerealFactory}.
 */
package com.booking.sereal.jackson;