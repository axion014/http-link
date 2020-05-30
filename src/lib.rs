use std::fmt;
use percent_encoding::percent_decode_str;

/*
 * WHATWG URL is equivalent of W3C URI with best effort handling for non-ASCII characters.
 * According to RFC 8288, it's fine.
 */
use url::Url;

#[derive(Debug, Eq, PartialEq)]
pub struct Link {
	pub target: Url,
	pub rel: String,
	pub context: Url,
	pub attributes: Vec<Parameter>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Parameter {
	pub name: String,
	pub value: String
}

#[derive(Debug)]
pub enum ParseLinkError {
	SyntaxError(String),
	InvaildUrl(url::ParseError),
	BadEncoding(std::str::Utf8Error),
	UnknownEncoding
}

use ParseLinkError::*;

impl fmt::Display for ParseLinkError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			SyntaxError(description) => write!(f, "Syntax error: {}", description),
			InvaildUrl(e) => write!(f, "{}", e),
			BadEncoding(e) => write!(f, "Bad charactor encoding in attributes: {}", e),
			UnknownEncoding => write!(f, "Unknown charactor encoding in attributes")
		}
    }
}

impl std::error::Error for ParseLinkError {}

fn is_http_whitespace(ch: char) -> bool {
	ch == ' ' || ch == '\t'
}

static UNIQUE_ATTRIBUTES: [&str; 4] = ["media", "title", "title*", "type"];

// https://tools.ietf.org/html/rfc8288#appendix-B.2
pub fn parse_link_header(s: &str, base: &Url) -> Result<Vec<Link>, ParseLinkError> {

	let mut links = Vec::new(); // 1.

	for mut v in s.split(',') { // 2.

		v = v.trim_start_matches(is_http_whitespace); // 2.1.

		if v.starts_with('<') {
			v = &v[1..]; // 2.3.
		} else {
			return Err(SyntaxError("Link doesn't start with a \"<\"".to_string())); // 2.2.
		};

		// 2.4/2.5.
		let (target_str, params_str) = v.split_at(v.find('>').ok_or(SyntaxError("Unclosed <".to_string()))?);

		let params = parse_params(&params_str[1..])?; // 2.6/2.7.

		let target = Url::options().base_url(Some(base)).parse(target_str).map_err(|e| InvaildUrl(e))?; // 2.8.

		// 2.9/2.10.
		let relations = params.iter().find(|p| p.name == "rel").map_or("", |p| &p.value)
			.split(is_http_whitespace).map(|s| s.to_string())
			.collect::<Vec<String>>();

		// 2.11/2.12.
		let anchor = params.iter().find(|p| p.name == "anchor");
		let context = if let Some(anchor) = anchor {
			Url::options().base_url(Some(base)).parse(&anchor.value).map_err(|e| InvaildUrl(e))?
		} else {
			base.to_owned()
		};

		let mut attributes_found = Vec::<String>::new();
		let mut attributes = params.into_iter().filter(|p| { // 2.13/2.14.
			if p.name == "rel" || p.name == "anchor" { return false; } // 2.14.1.
			if UNIQUE_ATTRIBUTES.iter().any(|n| n == &p.name) {
				if attributes_found.iter().any(|n| n == &p.name) {
					return false; // 2.14.2.
				} else {
					attributes_found.push(p.name.to_string());
					return true;
				}
			}
			true
		}).collect::<Vec<Parameter>>();

		// 2.15
		let star_param_names = attributes.iter()
			.filter_map(|p| if p.name.ends_with('*') { Some(p.name.to_string()) } else { None })
			.collect::<Vec<String>>();

		for star_param_name in star_param_names { // 2.16
			let base_param_name = &star_param_name[..(star_param_name.len() - 1)]; // 2.16.1.
			let tmp = attributes.into_iter().filter_map(|p| {
				if p.name == base_param_name { None } // 2.16.3.
				else {
					if p.name == star_param_name { // 2.16.4.
						Some(Parameter {
							name: base_param_name.to_string(),
							value: p.value
						})
					} else {
						Some(p)
					}
				}
			}).collect();
			attributes = tmp;
		}

		// 2.17
		for rel in relations.iter().map(|rel| rel.to_ascii_lowercase()) { // 2.17.1.
			links.push(Link { // 2.17.2.
				target: target.to_owned(),
				rel,
				context: context.to_owned(),
				attributes: attributes.to_owned()
			});
		}
	}
	Ok(links) // 3.
}

// https://tools.ietf.org/html/rfc8288#appendix-B.3
pub fn parse_params(mut s: &str) -> Result<Vec<Parameter>, ParseLinkError> {

	let mut params = Vec::new(); // 1.

	while !s.is_empty() { // 2.

		s = s.trim_start_matches(is_http_whitespace); // 2.1.

		if s.starts_with(';') {
			s = s[1..].trim_start_matches(is_http_whitespace); // 2.3.
		} else {
			return Err(SyntaxError("Expected parameter separator".to_string())); // 2.2.
		};

		s = s.trim_start_matches(is_http_whitespace); // 2.4.

		// 2.5/2.9.
		let split = s.split_at(s.find(|ch| is_http_whitespace(ch) || ch == '=' || ch == ';' || ch == ',')
			.ok_or(SyntaxError("Expected \"=\" or parameter separator".to_string()))?);
		let name = split.0.to_ascii_lowercase();

		s = split.1.trim_start_matches(is_http_whitespace); // 2.6.

		let value = if s.starts_with('=') { // 2.7.
			s = s[1..].trim_start_matches(is_http_whitespace); // 2.7.1/2.7.2.
			let value = if s.starts_with('"') { // 2.7.3. https://tools.ietf.org/html/rfc8288#appendix-B.4
				let mut v = String::new(); // 1.
				// 2. is skipped
				s = &s[1..]; // 3.
				while !s.starts_with('"') { // 4/4.2.

					if s.starts_with('\\') { s = &s[1..]; } // 4.1.

					v.push(s.chars().next().ok_or(SyntaxError("Unexpected end of input".to_string()))?); // 4.1.2.

					// 4.3/4.1.3.
					s = s.get(1..).ok_or(SyntaxError("Bad non-ASCII charactor detected".to_string()))?;
				}
				s = &s[1..];
				v
			} else { // 2.7.4.
				let split = s.split_at(s.find(|ch| ch == ';' || ch == ',').or(Some(s.len())).unwrap());
				s = split.1;
				split.0.to_string()
			};
			if name.ends_with('*') { // 2.7.5.
				let mut iter = value.split('\'');
				let (encoding, _lang, value) = (
					iter.next().unwrap(),
					iter.next().ok_or(SyntaxError("Expected \"'\"".to_string()))?,
					iter.next().ok_or(SyntaxError("Expected \"'\"".to_string()))?
				);
				if iter.next().is_some() { return Err(SyntaxError("Unexpected \"'\"".to_string())); }
				match encoding.to_ascii_uppercase().as_str() {
					"UTF-8" => percent_decode_str(value).decode_utf8().map_err(|e| BadEncoding(e))?.to_string(),
					_ => return Err(UnknownEncoding)
				}
			} else { value }
		} else { "".to_string() }; // 2.8.
		params.push(Parameter { name, value }); // 2.10.
	}
	Ok(params)
}

#[test]
fn rfc8288_examples() -> Result<(), ParseLinkError> {
	let base = Url::parse("http://example.com").unwrap();
	assert_eq!(
		parse_link_header(r#"<http://example.com/TheBook/chapter2>; rel="previous"; title="previous chapter""#, &base).unwrap(),
		vec![Link {
			target: Url::parse("http://example.com/TheBook/chapter2").unwrap(),
			rel: "previous".to_string(),
			context: base.clone(),
			attributes: vec![Parameter {
				name: "title".to_string(),
				value: "previous chapter".to_string()
			}]
		}]
	);
	assert_eq!(parse_link_header(r#"</>; rel="http://example.net/foo""#, &base).unwrap(), vec![Link {
		target: base.clone(),
		rel: "http://example.net/foo".to_string(),
		context: base.clone(),
		attributes: Vec::new()
	}]);
	assert_eq!(parse_link_header(r##"</terms>; rel="copyright"; anchor="#foo""##, &base).unwrap(), vec![Link {
		target: Url::parse("http://example.com/terms").unwrap(),
		rel: "copyright".to_string(),
		context: Url::parse("http://example.com#foo").unwrap(),
		attributes: Vec::new()
	}]);
	assert_eq!(parse_link_header("</TheBook/chapter2>; rel=\"previous\"; title*=UTF-8'de'letztes%20Kapitel, \
								</TheBook/chapter4>; rel=\"next\"; title*=UTF-8'de'n%c3%a4chstes%20Kapitel", &base).unwrap(),
		vec![Link {
			target: Url::parse("http://example.com/TheBook/chapter2").unwrap(),
			rel: "previous".to_string(),
			context: base.clone(),
			attributes: vec![Parameter {
				name: "title".to_string(),
				value: "letztes Kapitel".to_string()
			}]
		}, Link {
			target: Url::parse("http://example.com/TheBook/chapter4").unwrap(),
			rel: "next".to_string(),
			context: base.clone(),
			attributes: vec![Parameter {
				name: "title".to_string(),
				value: "nächstes Kapitel".to_string()
			}]
		}]
	);
	assert_eq!(parse_link_header(r#"<http://example.org/>; rel="start http://example.net/relation/other""#, &base).unwrap(),
		vec![Link {
			target: Url::parse("http://example.org/").unwrap(),
			rel: "start".to_string(),
			context: base.clone(),
			attributes: Vec::new()
		}, Link {
			target: Url::parse("http://example.org/").unwrap(),
			rel: "http://example.net/relation/other".to_string(),
			context: base,
			attributes: Vec::new()
		}]
	);
	Ok(())
}