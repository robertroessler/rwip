/*
	rmj.h - interface (AND implementation) of the RMj "mini" JSON parser

	Copyright(c) 2024, Robert Roessler
	All rights reserved.

	Redistribution and use in source and binary forms, with or without
	modification, are permitted provided that the following conditions are met:

	1. Redistributions of source code must retain the above copyright notice,
	this list of conditions and the following disclaimer.

	2. Redistributions in binary form must reproduce the above copyright notice,
	this list of conditions and the following disclaimer in the documentation
	and/or other materials provided with the distribution.

	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
	AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
	IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
	ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
	LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
	CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
	SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
	INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
	CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
	ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
	POSSIBILITY OF SUCH DAMAGE.
*/

#pragma once

#include <string>
#include <string_view>
#include <map>
#include <vector>
#include <charconv>
#include <stdexcept>
#include <type_traits>
#include "variant.hpp"

namespace rmj {

using namespace std::string_literals;
using namespace std::string_view_literals;

// (make sure we use c++14 "is_transparent" comparators)
template<typename K, typename T, typename Cmp = std::less<>>
using map = std::map<K, T, Cmp>;

/*
	Define the JSON "value" type as a *recursive* variant by using the most
	excellent rva::variant from the Recursive Variant Authority, available @

	https://github.com/codeinred/recursive-variant

	Note the use of the "placeholder" rva::self_t whenever you actually want
	a recursive reference to "yourself"... this is illustrated below in the
	definition of rmj::js_val_base.

	But also note the definitions of rmj::js_obj and rms::js_arr - once the
	initial recursive definition has been completed, you do not need to use
	or refer to rva::self_t again... we have a simulated recursive sum type
*/
using js_val_base = rva::variant<
	nullptr_t,
	bool,
	double,
	std::string,
	map<std::string, rva::self_t>,
	std::vector<rva::self_t>>;

// "convenience" typedefs for the non-literal js_val alternatives
using js_obj = map<std::string, js_val_base>;
using js_arr = std::vector<js_val_base>;
using js_num = double; // (useful for any conversion / precision questions)

// (internally used definitions not intended to appear in the rmj namespace)
namespace detail {
	/*
		Note that we are looking for very specific utf-8/ascii codepoints, so
		we do NOT want any "locale-mapping" to be taking place.
	*/
	constexpr auto isdigit(char8_t c) noexcept { return c >= '0' && c <= '9'; }
	constexpr auto isalpha(char8_t c) noexcept { return c >= 'a' && c <= 'z'; }

	/*
		sizeOfUTF8CodeUnits returns the length in bytes of a UTF-8 code point, based
		on being passed the [presumed] first byte.

		N.B. - if the passed value does NOT represent [the start of] a well-formed
		UTF-8 code point, the returned length is ZERO, which means this should most
		likely be used at least initially in a "validation" capacity.

		Conceptually, this is the logic:

		return
			isascii(c)                     ? 1 :
			(c & 0b11100000) == 0b11000000 ? 2 :
			(c & 0b11110000) == 0b11100000 ? 3 :
			(c & 0b11111000) == 0b11110000 ? 4 :
			0; // (caller(s) should NOTICE this)
	*/
	constexpr size_t sizeOfUTF8CodeUnits(int u) noexcept {
		return
			"\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1"	// 00-0f 1-byte UTF-8/ASCII
			"\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1"	// 10-1f
			"\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1"	// 20-2f
			"\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1"	// 30-3f
			"\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1"	// 40-4f
			"\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1"	// 50-5f
			"\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1"	// 60-6f
			"\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1"	// 70-7f

			"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"	// 80-8f <illegal>
			"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"	// 90-9f
			"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"	// a0-af
			"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"	// b0-bf

			"\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2"	// c0-cf 2-byte UTF-8
			"\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2\2"	// d0-df

			"\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3"	// e0-ef 3-byte UTF-8

			"\4\4\4\4\4\4\4\4"					// f0-f7 4-byte UTF-8

			"\0\0\0\0\0\0\0\0"					// f8-ff <illegal>
			[u & 0xff];
	}

	constexpr size_t sizeOfUTF16CodeUnits(int u) noexcept {
		return
			u < 0xd800 ? 1 :
//			u < 0xdc00 ? 2 : // (no need to distinguish this range separately)
			u < 0xe000 || u >= 0x10000 ? 2 :
			1;
	}

	/*
		codePointToUTF8 is a template function providing flexible output options for
		the encoded UTF-8 chars representing the supplied Unicode code point.

		It is a template function so you can choose to store the output UTF-8 stream
		either like this

		char buf[80], * s = buf;
		codePointToUTF8(c, [&](char x) { *s++ = x; })

		or this

		std::string buf;
		codePointToUTF8(c, [&](char x) { buf.push_back(x); })

		... where c is a Unicode code point in a char32_t.
	*/
	template<class CharOutput>
	constexpr void codePointToUTF8(char32_t c, CharOutput f) {
		if (c < 0x80)
			f((char)c);
		else if (c < 0x800)
			f((char)(0b11000000 | (c >> 6))),
			f((char)((c & 0b111111) | 0b10000000));
		else if (c < 0x10000)
			f((char)(0b11100000 | (c >> 12))),
			f((char)(((c >> 6) & 0b111111) | 0b10000000)),
			f((char)((c & 0b111111) | 0b10000000));
		else
			f((char)(0b11110000 | (c >> 18))),
			f((char)(((c >> 12) & 0b111111) | 0b10000000)),
			f((char)(((c >> 6) & 0b111111) | 0b10000000)),
			f((char)((c & 0b111111) | 0b10000000));
	}

	template<class WordOutput>
	constexpr void codePointToUTF16(char32_t c, WordOutput g) {
		if (c < 0xd800 || (c >= 0xe000 && c < 0x10000))
			g((wchar_t)c);
		else {
			const unsigned int v = c - 0x10000;
			g((wchar_t)(0xd800 | (v & 0x3ff))), g((wchar_t)(0xdc00 | (v >> 10)));
		}
	}

	constexpr char32_t codePointFromUTF8(const char* u) {
		switch (const char8_t c = u[0]; sizeOfUTF8CodeUnits(c)) {
		case 1: return c;
		case 2: return (c & 0b11111) << 6 | (u[1] & 0b111111);
		case 3: return (c & 0b1111) << 12 | (u[1] & 0b111111) <<  6 | (u[2] & 0b111111);
		case 4: return (c & 0b111)  << 18 | (u[1] & 0b111111) << 12 | (u[2] & 0b111111) << 6 | (u[3] & 0b111111);
		default: return 0; // (SHOULDN'T happen)
		}
		return 0; // ("CAN'T happen")
	}

	constexpr char32_t codePointFromUTF16(const wchar_t* u) {
		return
			sizeOfUTF16CodeUnits(u[0]) == 1 ? u[0] :
			((u[0] - 0xd800) << 10) + (u[1] - 0xdc00) + 0x10000;
	}

	constexpr std::string utf8StringFromUTF16(const wchar_t* u) {
		std::string t;
		while (*u)
			codePointToUTF8(codePointFromUTF16(u), [&t](char c) { t.push_back(c); }),
			u += sizeOfUTF16CodeUnits(*u);
		return t;
	}

	constexpr std::wstring utf16StringFromUTF8(const char* u) {
		std::wstring t;
		while (*u)
			codePointToUTF16(codePointFromUTF8(u), [&t](wchar_t c) { t.push_back(c); }),
			u += sizeOfUTF8CodeUnits(*u);
		return t;
	}

	// (Herb Sutter's [presumably] portable "trick" to make compilers shut up)
	template<class T> void ignore(const T&) {}

	// allow *assignment* of actual *numbers* (which means NO nullptrs or bools)
	template <class T>
	concept numeric =
	(std::integral<T> || std::floating_point<T>) &&
	!(std::same_as<T, nullptr_t> || std::same_as<T, bool>);
}

class js_val : public js_val_base {
	// collections of derived js_val forms, giving access to public methods
	using js_obj_ext = map<std::string, js_val>;
	using js_arr_ext = std::vector<js_val>;

public:
	// various ctors, "converting" and otherwise
	js_val() = default;
	js_val(const js_val&) = default;
	constexpr js_val(const js_val_base& v) { get_base() = v; }
	constexpr js_val(js_val_base&& v) { get_base() = v; }

	constexpr js_val(nullptr_t v) { get_base() = v; }
	constexpr js_val(bool v) { get_base() = v; }
	// coerce ANY "numeric" value (see detail::numeric concept) to underlying
	// JSON/js_val numeric type of [IEEE 754] double
	constexpr js_val(detail::numeric auto v) { get_base() = (double)v; }
	constexpr js_val(std::string v) { get_base() = v; }
	/*constexpr*/ js_val(js_obj v) { get_base() = v; }
	constexpr js_val(js_arr v) { get_base() = v; }

	// query the current type held in our variant / "sum type"
	constexpr auto is_null() const noexcept { return std::holds_alternative<nullptr_t>(get_base()); }
	constexpr auto is_bool() const noexcept { return std::holds_alternative<bool>(get_base()); }
	constexpr auto is_num() const noexcept { return std::holds_alternative<double>(get_base()); }
	constexpr auto is_string() const noexcept { return std::holds_alternative<std::string>(get_base()); }
	constexpr auto is_obj() const noexcept { return std::holds_alternative<js_obj>(get_base()); }
	constexpr auto is_arr() const noexcept { return std::holds_alternative<js_arr>(get_base()); }

	// return ref to the requested type in our variant / "sum type"
	constexpr const auto& as_null() const { return std::get<nullptr_t>(get_base()); }
	constexpr auto& as_null() { return std::get<nullptr_t>(get_base()); }
	constexpr const auto& as_bool() const { return std::get<bool>(get_base()); }
	constexpr auto& as_bool() { return std::get<bool>(get_base()); }
	constexpr const auto& as_num() const { return std::get<double>(get_base()); }
	constexpr auto& as_num() { return std::get<double>(get_base()); }
	constexpr const auto& as_string() const { return std::get<std::string>(get_base()); }
	constexpr auto& as_string() { return std::get<std::string>(get_base()); }
	/*constexpr const auto& as_obj() const { return (js_obj_ext&)std::get<js_obj>(get_base()); }*/
	constexpr auto& as_obj() { return (js_obj_ext&)std::get<js_obj>(get_base()); }
	constexpr const auto& as_arr() const { return (js_arr_ext&)std::get<js_arr>(get_base()); }
	constexpr auto& as_arr() { return (js_arr_ext&)std::get<js_arr>(get_base()); }

	// "convenience" operators for element access in js_obj and js_arr collections
	// N.B. - these will FORCE the map/vector alternatives respectively, be aware!
	auto& operator[](const std::string& s) { return as_obj()[s]; }
	auto& operator[](const char* s) { return as_obj()[s]; }
	constexpr const auto& operator[](std::integral auto i) const { return as_arr()[i]; }
	constexpr auto& operator[](std::integral auto i) { return as_arr()[i]; }

private:
	// defs supporting the implementation of RMj's parse() and to_string()...
	enum class parse_state { free, in_number, in_keyword, in_string, in_object,
		obj_colon, end_object, in_array, end_array, more_items, eod, illegal };

	using val_or_state = std::variant<js_val, parse_state>;

	// ... including the below riffs on "is" and "as" used above...
	constexpr static auto has_state(auto v) noexcept { return std::holds_alternative<parse_state>(v); }
	constexpr static auto state(auto v) { return std::get<parse_state>(v); }

	constexpr static auto has_val(auto v) noexcept { return std::holds_alternative<js_val>(v); }
	constexpr static auto val(auto v) { return std::get<js_val>(v); }

	// ... and the mysterious and magical "overload" template for std::visit()
	template<class... Ts> struct overload : Ts... { using Ts::operator()...; };

public:
	/*
		Create an external string representation that is presumably valid RFC 8259
		JSON text... the chances of this being the case are MUCH higher if the parsed
		[internal] representation was itself created by parsing a valid RFC 8259 doc.

		A special note on anyone doing "round trip" testing: while parsing the UTF-8
		input, if any UTF-16 "escaped" characters are seen, they will be converted to
		their utf-8 encoding forms... this applies to both Basic Multilingual Plane
		characters as well as any utf-16 "surrogate pairs".  This means that utf-16
		"escaped" characters that were present prior to parsing will, on output by
		rmj::to_string, appear as their EXACTLY EQUIVALENT utf-8 forms... which MAY
		include some "control characters" (U+0000 - U+001F) represented as "\u00nn".

		As an additional note on "round trips", any JSON text that included a BOM at
		the beginning was politely accepted, after IGNORING said BOM... and under no
		circumstances is any attempt made to remember this and include it in the new
		version of the JSON text produced by to_string, as this would be in violation
		of RFC 8259 - and a Bad Idea(tm) in general.
	*/
	constexpr std::string to_string() {
		using namespace detail;
		// return external form of JSON "string"
		auto string_of_string = [&](std::string_view v) {
			size_t co{};
			std::string o;
			o.reserve(256);
			o.push_back('"');
			while (co < v.size())
				if (auto n = sizeOfUTF8CodeUnits((char8_t)v[co]); n <= 1) {
					if (!n)
						throw std::runtime_error("Bad stringify (STRING: invalid utf-8 sequence)"s);
					switch (const auto c = (char8_t)v[co++]; c) {
					case 0x08: o.append("\\b"sv); break;
					case 0x09: o.append("\\t"sv); break;
					case 0x0a: o.append("\\n"sv); break;
					case 0x0c: o.append("\\f"sv); break;
					case 0x0d: o.append("\\r"sv); break;
					case 0x22: o.append("\\\""sv); break;
					case 0x5c: o.append("\\\\"sv); break;
					default:
						if (c < 0x20) {
							char b[]{ '\\', 'u', '0', '0', '0', '0' };
							const auto s = c < 0x10 ? 5 : 4;
							auto [p, e] = std::to_chars(b + s, b + 6, c, 16);
							ignore(p), ignore(e);
							o.append(b, 6);
						} else
							o.push_back(c);
						break;
					}
				} else
					while (n--)
						o.push_back(v[co++]);
			o.push_back('"');
			return o;
		};
		// return external form of JSON "object"
		auto string_of_obj = [&](const auto& v) {
			std::string o;
			o.reserve(256);
			o.push_back('{');
			for (const auto& [key, val] : v)
				o.append(o.size() == 1 ? "\""sv : ",\""sv)
				.append(key)
				.append("\":"sv)
				.append(js_val(val).to_string());
			o.push_back('}');
			return o;
		};
		// return external form of JSON "array"
		auto string_of_arr = [&](const auto& v) {
			std::string o;
			o.reserve(256);
			o.push_back('[');
			for (const auto& e : v) {
				if (o.size() > 1)
					o.push_back(',');
				o.append(js_val(e).to_string());
			}
			o.push_back(']');
			return o;
		};
		// return external form of JSON "value"
		return std::visit(overload{
			[&](nullptr_t) { return "null"s; },
			[&](bool) { return as_bool() ? "true"s : "false"s; },
			[&](double) {
				char b[24];
				const auto [p, e] = std::to_chars(b, b + std::size(b), as_num());
				return std::string{ b, p };
			},
			[&](const std::string&) { return string_of_string(as_string()); },
			[&](const js_obj&) { return string_of_obj(as_obj()); },
			[&](const js_arr&) { return string_of_arr(as_arr()); }
		}, get_base());
	}

	/*
		Fully parse the supplied source string_view into an in-memory rmj::js_val,
		as specified in RFC 8259 (which obsoletes 4627, 7158, and 7159)... it should
		be noted that rmj::parse accepts "bare" values, not only objects or arrays.

		We expect the input to be encoded as utf-8 (which, of course, means ASCII is
		accepted).  But do note the comment on rmj::to_string above relating to, for
		instance, "round trips".

		Attempt to detect badly-formed JSON, and throw std::runtime_error exceptions
		that include explanatory ".what()" text that includes the BYTE offset in the
		incoming source string_view where the parser detected the offending sequence.
	*/
	static js_val parse(std::string_view src) {
		using namespace detail;
		size_t co{}; // ("current offset")
		// classifier: JSON "whitespace"
		constexpr auto is_ws = [](auto c) noexcept { return c == ' ' || c == '\n' || c == '\r' || c == '\t'; };
		// skip over "whitespace", co -> 1st NON-whitespace
		auto ws = [&]() noexcept {
			while (co < src.size() && is_ws(src[co]))
				++co;
		};
		// skip over "digits", co -> 1st NON-digit
		auto digits = [&]() noexcept {
			while (++co < src.size() && isdigit(src[co])) ;
		};
		// skip over JSON "keyword" chars, co -> 1st NON-alpha
		auto alphas = [&]() noexcept {
			while (++co < src.size() && isalpha(src[co])) ;
		};
		// parse JSON "number", converting to IEEE 64-bit float (aka "double")
		auto number = [&]() {
			// classifier: [tokens ending] JSON "number"
			constexpr auto is_eon = [](auto c) noexcept { return c == ',' || c == '}' || c == ']'; };
			const auto start{ co };
			double d{};
			digits();
			if (co >= src.size() || is_ws(src[co]) || is_eon(src[co])) {
				// legal number, i.e., NO leading zero?
				if (src[start] == '0' && (co - start) > 1)
					throw std::runtime_error("Bad parse (NUMBER) @ "s + std::to_string(start));
				// have integral value
				const auto [p, e] = std::from_chars(src.data() + start, src.data() + co, d);
				ignore(p), ignore(e);
				return d;
			}
			if (src[co] == '.')
				digits();
			if (co >= src.size() || is_ws(src[co]) || is_eon(src[co])) {
				// have fixed-point value
				const auto [p, e] = std::from_chars(src.data() + start, src.data() + co, d);
				ignore(p), ignore(e);
				return d;
			}
			if (auto c = src[co]; c == 'e' || c == 'E') {
				if (++co >= src.size())
					throw std::runtime_error("Bad parse (NUMBER) @ "s + std::to_string(co));
				if (c = src[co]; c == '+' || c == '-')
					++co;
				if (co >= src.size() || is_ws(src[co]))
					throw std::runtime_error("Bad parse (NUMBER) @ "s + std::to_string(co));
				digits();
				// have fixed-point value WITH exponent
				const auto [p, e] = std::from_chars(src.data() + start, src.data() + co, d);
				ignore(p), ignore(e);
				return d;
			}
			throw std::runtime_error("Bad parse (NUMBER) @ "s + std::to_string(co));
		};
		// parse JSON "keyword"
		auto keyword = [&]() {
			const auto start{ co };
			alphas();
			if (const auto t{ src.substr(start, co - start) }; t == "null"sv)
				return js_val{ nullptr };
			else if (t == "true"sv)
				return js_val{ true };
			else if (t == "false"sv)
				return js_val{ false };
			throw std::runtime_error("Bad parse ([unexpected] KEYWORD) @ "s + std::to_string(start));
		};
		// parse JSON "string", converting to utf-8 encoding
		auto string = [&]() {
			// (handle utf-16 Basic Multilingual Plane as well as surrogate pairs)
			auto utf16 = [&]() {
				if (co + 5 >= src.size())
					throw std::runtime_error("Bad parse (STRING: invalid utf-16 sequence) @ "s + std::to_string(co - 1));
				uint16_t u[2];
				++co;
				const auto [p, e] = std::from_chars(src.data() + co, src.data() + co + 4, u[0], 16);
				ignore(p), ignore(e);
				co += 4;
				if (sizeOfUTF16CodeUnits(u[0]) > 1)
					if (co + 6 >= src.size() || src[co] != '\\' || src[co + 1] != 'u')
						throw std::runtime_error("Bad parse (STRING: invalid utf-16 surrogate pair) @ "s + std::to_string(co));
					else {
						co += 2;
						const auto [p, e] = std::from_chars(src.data() + co, src.data() + co + 4, u[1], 16);
						ignore(p), ignore(e);
						co += 4;
					}
				return codePointFromUTF16((wchar_t*)u);
			};
			std::string o;
			o.reserve(256);
			++co;
			while (co < src.size() && src[co] != '"')
				if (src[co] == '\\') {
					switch (src[++co]) {
					case '"': o.push_back('"'); break;
					case '\\': o.push_back('\\'); break;
					case '/': o.push_back('/'); break;
					case 'b': o.push_back('\b'); break;
					case 'f': o.push_back('\f'); break;
					case 'n': o.push_back('\n'); break;
					case 'r': o.push_back('\r'); break;
					case 't': o.push_back('\t'); break;
					case 'u':
						codePointToUTF8(utf16(), [&](auto x) { o.push_back(x); });
						--co; // (pre-compensate for below "++co")
						break;
					default:
						throw std::runtime_error("Bad parse (STRING: invalid escaped char) @ "s + std::to_string(co));
					}
					++co;
				} else {
					auto n = sizeOfUTF8CodeUnits((char8_t)src[co]);
					if (!n || co + n > src.size())
						throw std::runtime_error("Bad parse (STRING: invalid utf-8 sequence) @ "s + std::to_string(co));
					while (n--)
						o.push_back(src[co++]);
				}
			if (co >= src.size() || src[co++] != '"')
				throw std::runtime_error("Bad parse (STRING: invalid termination) @ "s + std::to_string(co));
			return o;
		};
		// parse [next] JSON token, returning EITHER "value" OR "parse state"
		auto next_token = [&]() -> val_or_state {
			using enum parse_state;
			if (ws(); co >= src.size())
				return eod;
			switch (src[co]) {
			case '"':
				return string();
			case ',':
				return more_items;
			case '-':
			case '0': case '1': case '2': case '3': case '4':
			case '5': case '6': case '7': case '8': case '9':
				return number();
			case ':':
				return obj_colon;
			case '[':
				return in_array;
			case ']':
				return end_array;
			case 'f': case 'n': case 't':
				return keyword();
			case '{':
				return in_object;
			case '}':
				return end_object;
			default:
				return illegal;
			}
		};
		// primary [recursive] parser, returning JSON "value"
		auto parse_impl = [&](auto&& parse_impl) -> js_val {
			js_val value{};
			if (auto t = next_token(); has_val(t))
				value = val(t);
			else
				switch (state(t)) {
				using enum parse_state;
				case in_object: {
					// (store JSON "object" as C++ std::map of JSON "values")
					value = js_obj();
					auto& mr = value.as_obj();
					do {
						++co, t = next_token();
						// check for [and ALLOW] "empty" object
						if (has_state(t) && state(t) == end_object && mr.empty())
							break;
						else if (!has_val(t) || !val(t).is_string())
							throw std::runtime_error("Bad parse (OBJECT: expected STRING) @ "s + std::to_string(co));
						else if (const auto u = next_token(); !has_state(u) || state(u) != obj_colon)
							throw std::runtime_error("Bad parse (OBJECT: expected ':') @ "s + std::to_string(co));
						const auto v = (++co, parse_impl(parse_impl));
						mr.try_emplace(val(t).as_string(), v);
						if (t = next_token(); has_state(t) && state(t) == end_object)
							break;
					} while (has_state(t) && state(t) == more_items);
					if (!has_state(t) || state(t) != end_object)
						throw std::runtime_error("Bad parse (OBJECT: expected ',' or '}') @ "s + std::to_string(co));
					++co; // (consume '}')
					break;
				}
				case in_array: {
					// (store JSON "array" as C++ std::vector of JSON "values")
					value = js_arr();
					auto& ar = value.as_arr();
					do {
						++co, t = next_token();
						// check for [and ALLOW] "empty" array
						if (has_state(t) && state(t) == end_array && ar.empty())
							break;
						else if (has_state(t) && (state(t) != in_object && state(t) != in_array))
							throw std::runtime_error("Bad parse (expected VALUE) @ "s + std::to_string(co));
						const auto v = has_val(t) ? val(t) : parse_impl(parse_impl);
						ar.emplace_back(v);
						if (t = next_token(); has_state(t) && state(t) == end_array)
							break;
					} while (has_state(t) && state(t) == more_items);
					if (!has_state(t) || state(t) != end_array)
						throw std::runtime_error("Bad parse (ARRAY: expected ',' or ']') @ "s + std::to_string(co));
					++co; // (consume ']')
					break;
				}
				default:
					// we MUST return a VALUE!
					throw std::runtime_error("Bad parse (expected VALUE) @ "s + std::to_string(co));
				}
			return value;
		};
		// Detect and IGNORE utf-8 BOM at beginning of JSON text... (not reversible)
		if (src.size() >= 3 &&
			(char8_t)src[0] == 0xEF &&
			(char8_t)src[1] == 0xBB &&
			(char8_t)src[2] == 0xBF)
			co += 3;
		return parse_impl(parse_impl);
	}
};

// "user-defined suffix" HELPFUL when using "bare" INTEGERS in js_val exprs...
// the alternative is using casts - think, (js_num)0, (js_num)1, (js_num)2 vs:
// e.g., js_arr a{0, 1, 2} => js_arr a{0_js, 1_js, 2_js}, whereas
// e.g., js_arr a{0.0, 1.0, 2.0} is correctly defined/legal as is
constexpr auto operator""_js(unsigned long long v) noexcept { return (js_num)v; }

}