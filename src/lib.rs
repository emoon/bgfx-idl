use full_moon::{ast::punctuated::Pair, ast::*, tokenizer::*, visitors::Visitor};
use std::fs;

/// Holds an entry in a table
#[derive(Debug, Default)]
pub struct TableEntry {
    /// Name of the entry
    pub name: String,
    /// String data if ("" if no string data)
    pub str_data: String,
    /// Integer value (None if no value)
    pub num: Option<i64>,
}

/// Holds data for the table entries
type TableData = Vec<TableEntry>;

/// Holds an entry and a comment with line and pos start
#[derive(Debug, Default, Clone)]
pub struct Comment {
    /// Position on the line (starting with 1)
    pub pos: usize,
    /// The actual data
    pub text: String,
}

/// A coms that logs every local assignment made
#[derive(Default)]
pub struct GatherComments {
    /// Comments
    pub comments: Vec<Option<Comment>>,
    /// Each line with code will be marked as true
    pub codelines: Vec<bool>,
}

/// Holds an entry and a comment for it
#[derive(Debug, Default)]
pub struct EntryLine {
    /// the entry on the line
    pub text: String,
    /// line number of the data
    pub line: usize,
    /// Comment(s) for the line/belonging lines
    pub comment: String,
}

/// Arugments for functions/structs
#[derive(Debug, Default)]
pub struct FuncArg {
    /// Name and line
    pub name_line: EntryLine,
    /// Type of the argument
    pub arg_type: Type,
    /// Table data (such as out and other modifiers)
    pub table: TableData,
    /// temporary type_name, see arg_type for proper data
    pub type_name: String,
}

/// Holds data for a func/struct.x
#[derive(Debug, Default)]
pub struct Func {
    /// Comment(s) before the function
    pub comments: String,
    /// name of the function
    pub name: EntryLine,
    /// name of the class (like func.VertexBuffer this would be VertexBuffer)
    pub class: EntryLine,
    /// Table data for "settings" of the function
    pub table: TableData,
    /// Which line the return statement appears on
    pub return_name_line: EntryLine,
    /// Return type of the function
    pub return_type: Type,
    /// function arguments
    pub args: Vec<FuncArg>,
}

/// Arugments for functions/structs
#[derive(Debug, Default)]
pub struct FlagArg {
    /// Name and line
    pub name_line: EntryLine,
    /// actual value of the flag. Overriden if there is a table
    pub value: Option<u64>,
    /// Table data (such as out and other modifiers)
    pub table: Option<TableData>,
}

/// Holds data for a flag/enum
#[derive(Debug, Default)]
pub struct Flag {
    /// Type in bits (such as 64, 32, 16, 8)
    pub size: usize,
    /// Comment(s) before the flag/enum
    pub comments: String,
    /// name of the flag/enum
    pub name: EntryLine,
    /// Table data for "settings" of the flag
    pub table: Option<TableData>,
    /// entries for the flag
    pub entries: Vec<FlagArg>,
}

/// Holds data for a typedef
#[derive(Debug, Default)]
pub struct Typedef {
    /// Type and name
    pub type_line: EntryLine,
    /// Additional data
    pub table: Option<TableData>,
}

/// Holds all the parsed data
#[derive(Debug, Default)]
pub struct Idl {
    /// Version of the file
    pub version: Option<u64>,
    /// Typedefs (such as uint32_t, void, etc)
    pub typedefs: Vec<Typedef>,
    /// Handles
    pub handles: Vec<Typedef>,
    /// Functions
    pub funcs: Vec<Func>,
    /// Structs
    pub structs: Vec<Func>,
    /// Functions
    pub enums: Vec<Flag>,
    /// Structs
    pub flags: Vec<Flag>,
}

/// Holds info on a type
#[derive(Debug, PartialEq)]
pub enum VarType {
    /// Default value, should never appear in code
    Unknown(String),
    /// TypeName that is an enum
    Enum(String),
    /// TypeName that is an enum
    Struct(String),
    /// Such as uint8_t, uint16_t, bool, etc
    Primitive(String),
    /// Array with type_name and count
    Array(String, String),
}

/// Holds info on a type
#[derive(Debug, Default)]
pub struct Type {
    pub var_type: VarType,
    /// if type is const
    pub is_const: bool,
    /// if type is a reference
    pub is_ref: bool,
    /// if type is a output parameter
    pub is_output: bool,
    /// if type is a pointer
    pub is_pointer: bool,
}

impl Default for VarType {
    fn default() -> Self {
        VarType::Unknown(String::new())
    }
}

impl Type {
    fn primitive(name: &str) -> Type {
        Type {
            var_type: VarType::Primitive(name.to_owned()),
            ..Default::default()
        }
    }
}

// Holds flag attributes to calculate the flag values
#[derive(Debug, Default)]
struct FlagAttributes {
    bits: u64,
    shift: u64,
    range: u64,
    base: u64,
    is_const: bool,
}

fn get_identifier<'a>(token_ref: &'a TokenReference) -> Option<&'a str> {
    match token_ref.token().token_type() {
        TokenType::Identifier { identifier } => Some(identifier),
        TokenType::Symbol { symbol } => {
            match symbol {
                Symbol::True => Some("true"),
                Symbol::False => Some("false"),
                _ => None,
            }
        }
        _ => None,
    }
}

fn get_token_number<'a>(token_ref: &'a TokenReference) -> Option<&'a str> {
    match token_ref.token().token_type() {
        TokenType::Number { text } => Some(text),
        _ => None,
    }
}

fn get_token_string(token: &TokenReference) -> EntryLine {
    let mut entry = EntryLine::default();

    match token.token().token_type() {
        TokenType::StringLiteral {
            literal,
            multi_line: _,
            quote_type: _,
        } => entry.text = literal.to_owned().to_string(),
        TokenType::Identifier { identifier } => entry.text = identifier.to_owned().to_string(),
        _ => {
            panic!();
        }
    }

    entry.line = token.token().start_position().line();
    entry
}

fn get_token_var_string(var: &Var) -> EntryLine {
    match var {
        Var::Name(name) => get_token_string(name),
        _ => panic!(),
    }
}

fn get_value(entry: &mut TableEntry, value: &Value) {
    match value {
        Value::Number(value) => {
            let val = get_token_number(value).unwrap();
            // handle hex data
            if let Some(hex) = val.strip_prefix("0x") {
                entry.num = Some(i64::from_str_radix(hex, 16).unwrap());
            } else {
                entry.num = Some(val.parse().unwrap());
            }
        }

        Value::String(value) => {
            let t = get_token_string(value);
            entry.str_data = t.text;
        }

        Value::Symbol(value) => {
            if let Some(v) = get_identifier(value) {
                entry.str_data = v.to_owned();
            }
        }

        Value::Var(var) => {
            let t = get_token_var_string(var);
            entry.str_data = t.text.to_owned();
        }

        _ => (),
    }
}

fn get_expression(entry: &mut TableEntry, expr: &Expression) {
    if let Expression::Value { value } = expr {
        get_value(entry, value)
    }
}

fn get_string_value(value: &Value) -> EntryLine {
    match value {
        Value::String(value) => get_token_string(value),
        Value::Var(var) => get_token_var_string(var),
        _ => {
            dbg!(value);
            panic!();
        }
    }
}

fn get_expression_literal(expr: &Expression) -> EntryLine {
    match expr {
        Expression::Value { value } => get_string_value(value),
        _ => EntryLine::default(),
    }
}

fn get_table_data(table_data: &TableConstructor) -> Option<TableData> {
    let mut data = TableData::default();

    for t in table_data.fields() {
        match t {
            Field::NameKey {
                key,
                equal: _,
                value,
            } => {
                let mut entry = TableEntry {
                    name: get_identifier(key).unwrap().to_owned(),
                    ..Default::default()
                };

                get_expression(&mut entry, value);

                data.push(entry);
            }

            Field::NoKey(expr) => {
                let mut entry = TableEntry::default();
                let t = get_expression_literal(expr);
                entry.name = t.text;
                data.push(entry);
            }

            _ => (),
        }
    }

    Some(data)
}

fn get_par_data(expr: &Expression) -> Option<TableData> {
    let mut table = TableData::default();
    let mut table_entry = TableEntry::default();
    get_expression(&mut table_entry, expr);

    if !table_entry.str_data.is_empty() || table_entry.num.is_some() {
        table_entry.name = "paran".to_owned();
        table.push(table_entry);
        Some(table)
    } else {
        None
    }
}

fn fn_handle_anon_call<'a>(args: &'a FunctionArgs<'a>) -> Option<TableData> {
    match args {
        FunctionArgs::TableConstructor(table) => get_table_data(table),
        FunctionArgs::Parentheses {
            parentheses: _,
            arguments,
        } => {
            if let Some(t) = arguments.last() {
                match t {
                    Pair::End(t) => get_par_data(t),
                    _ => None,
                }
            } else {
                None
            }
        }
        _ => None,
    }
}

fn get_prefix_identifier<'a>(prefix: &'a Prefix) -> &'a str {
    match prefix {
        Prefix::Name(name) => match name.token().token_type() {
            TokenType::Identifier { identifier } => identifier,
            _ => "",
        },
        _ => "",
    }
}

// Check if current suffix is a table
fn get_table(s: &Suffix) -> Option<TableData> {
    match s {
        Suffix::Call(Call::AnonymousCall(args)) => fn_handle_anon_call(args),
        _ => None,
    }
}

// Get .<id/name> and also handle the special case of name["string"]
fn get_dot_name(s: &Suffix) -> EntryLine {
    match s {
        Suffix::Index(index) => match index {
            Index::Dot { dot: _, name } => get_token_string(name),
            Index::Brackets {
                brackets: _,
                expression,
            } => get_expression_literal(expression),
            _ => EntryLine::default(),
        },
        _ => EntryLine::default(),
    }
}

// Get string as arg
fn get_arg_string(s: &Suffix) -> EntryLine {
    match s {
        Suffix::Call(Call::AnonymousCall(FunctionArgs::String(token))) => get_token_string(token),
        _ => EntryLine::default(),
    }
}

// Get the start comment line for a top entry (struct, func, enum, etc)
// This works by working backwards from the starting line until no comment
// was found. The comments also has to be on the very left edge (pos 1)
fn get_top_start_comment_lines(gather_com: &mut GatherComments, line_start: usize) -> String {
    let line_start = line_start - 1;
    let mut line = line_start;
    let mut comment = String::new();

    loop {
        if let Some(line_data) = gather_com.comments[line].as_ref() {
            if line == 0 || line_data.pos != 1 {
                break;
            }
        } else {
            break;
        }

        line -= 1;
    }

    for l in line + 1..=line_start {
        comment.push_str(&gather_com.comments[l].as_ref().unwrap().text);
        comment.push('\n');
    }

    comment
}

// Get comments for the lines. The way this works is that it will search forward
// from the current line until it finds a marked codeline or a line that starts
// at the pos 1 (which is a function/struct/enum comment)
fn get_comments(entry: &mut EntryLine, gather_com: &mut GatherComments) {
    let mut comment = String::new();
    let mut line = entry.line;
    let max_len = gather_com.comments.len();

    loop {
        if let Some(entry) = gather_com.comments[line].as_ref() {
            comment.push_str(&entry.text);
            comment.push('\n');
        }

        line += 1;

        if line == max_len {
            break;
        }

        if let Some(entry) = gather_com.comments[line].as_ref() {
            if entry.pos == 1 {
                break;
            }
        }

        if gather_com.codelines[line] {
            break;
        }
    }

    entry.comment = comment;
}

// Add comments to the function from the comments-prepass
fn update_comments(func: &mut Func, gather_com: &mut GatherComments) {
    // we start with the getting the comments for the function
    func.comments = get_top_start_comment_lines(gather_com, func.name.line);

    // all the lines that is being used by code so we know the ranges of
    gather_com.codelines[func.return_name_line.line] = true;

    for arg in &func.args {
        gather_com.codelines[arg.name_line.line] = true;
    }

    // get comments for the lines
    get_comments(&mut func.return_name_line, gather_com);

    for arg in &mut func.args {
        get_comments(&mut arg.name_line, gather_com);
    }
}

// Add comments to the function from the comments-prepass
fn update_flag_comments(flag: &mut Flag, gather_com: &mut GatherComments) {
    // we start with the getting the comments for the function
    flag.comments = get_top_start_comment_lines(gather_com, flag.name.line);

    for ent in &flag.entries {
        gather_com.codelines[ent.name_line.line] = true;
    }

    for ent in &mut flag.entries {
        get_comments(&mut ent.name_line, gather_com);
    }
}

// get bits from the table useable data
fn get_bits_from_table(table: &[TableEntry]) -> FlagAttributes {
    let mut attribs = FlagAttributes::default();

    for f in table {
        match f.name.as_str() {
            "const" => attribs.is_const = true,
            "bits" => attribs.bits = f.num.unwrap() as u64,
            "base" => attribs.base = f.num.unwrap() as u64,
            "range" => attribs.range = f.num.unwrap() as u64,
            "shift" => attribs.shift = f.num.unwrap() as u64,
            _ => (),
        }
    }

    attribs
}

// get a typedef
fn parse_typedef_handle(in_func: &FunctionCall, _gather_com: &mut GatherComments) -> Typedef {
    let mut typedef = Typedef::default();
    let mut iter = in_func.suffixes();

    if let Some(it) = iter.next() {
        typedef.type_line = get_arg_string(it);

        if let Some(it) = iter.next() {
            typedef.table = get_table(it);
        }
    }

    typedef
}

// Get the version of the file version(x)
fn get_version(in_func: &FunctionCall) -> Option<u64> {
    let mut iter = in_func.suffixes();

    if let Some(it) = iter.next() {
        if let Some(table) = get_table(it) {
            return Some(table[0].num.unwrap() as u64);
        }
    }

    None
}

// Parse a enum/flag
fn parse_enum_or_flag(
    in_func: &FunctionCall,
    gather_com: &mut GatherComments,
    is_flag: bool,
) -> Flag {
    enum State {
        Name,
        Table,
        Arg,
    }

    let mut flag = Flag::default();
    let mut state = State::Name;
    let mut attribs = FlagAttributes::default();
    let mut value = 0u64;
    let mut arg = FlagArg::default();

    for s in in_func.suffixes() {
        match state {
            State::Name => {
                flag.name = get_dot_name(s);
                state = State::Table;
            }

            State::Table => {
                let table = get_table(s);
                if is_flag {
                    if let Some(table) = table.as_ref() {
                        attribs = get_bits_from_table(&table);
                        value = attribs.base << attribs.shift;
                        flag.size = attribs.bits as usize;
                    } else {
                        panic!(
                            "flag.{} doesn't have array of bit settings.",
                            flag.name.text
                        );
                    }
                }

                flag.table = table;
                state = State::Arg;
            }

            State::Arg => {
                let arg_table = get_table(s);

                // check if data is a table and do it's setup
                if let Some(table) = arg_table.as_ref() {
                    if table[0].name == "paran" {
                        let v = table[0].num.unwrap() as u64;
                        let v = if attribs.is_const || v == 0 {
                            v
                        } else {
                            1u64 << (v - 1)
                        };

                        arg.value = Some(v);
                    } else {
                        arg.value = None;
                        arg.table = arg_table;
                    }
                    flag.entries.push(arg);
                    arg = FlagArg::default();
                } else {
                    if !arg.name_line.text.is_empty() {
                        flag.entries.push(arg);
                        arg = FlagArg::default();
                    }

                    arg.name_line = get_dot_name(s);
                    arg.value = Some(value);

                    if attribs.shift != 0 || !is_flag {
                        value += 1u64 << attribs.shift;
                    } else {
                        value <<= 1;

                        if value == 0 {
                            value = 1;
                        }
                    }
                }
            }
        }
    }

    // make sure to add the last arg if we have any
    if !arg.name_line.text.is_empty() {
        flag.entries.push(arg);
    }

    // Update comments
    update_flag_comments(&mut flag, gather_com);

    flag
}

// Get detailed info on a type
fn get_detailed_type(type_name: &str, is_output: bool) -> Type {
    let mut ret_type = Type::default();
    let mut type_name = type_name;

    match type_name {
        e @ "bool" => return Type::primitive(e),
        e @ "char" => return Type::primitive(e),
        e @ "float" => return Type::primitive(e),
        e @ "int8_t" => return Type::primitive(e),
        e @ "int16_t" => return Type::primitive(e),
        e @ "int32_t" => return Type::primitive(e),
        e @ "int64_t" => return Type::primitive(e),
        e @ "uint8_t" => return Type::primitive(e),
        e @ "uint16_t" => return Type::primitive(e),
        e @ "uint32_t" => return Type::primitive(e),
        e @ "uint64_t" => return Type::primitive(e),
        e @ "void" => return Type::primitive(e),
        _ => (),
    }

    // Notice: this code doesn't support porinters or refs in arrays

    ret_type.is_output = is_output;
    ret_type.is_const = type_name.starts_with("const");

    if ret_type.is_const {
        type_name = &type_name[6..];
    }

    // Check if it's an array
    let a0 = type_name.find('[');
    let a1 = type_name.find(']');

    if let (Some(a), Some(b)) = (a0, a1) {
        ret_type.var_type =
            VarType::Array(type_name[0..a].to_owned(), type_name[a + 1..b].to_owned());
        return ret_type;
    }

    ret_type.is_ref = type_name.ends_with('&');
    ret_type.is_pointer = type_name.ends_with('*');

    if ret_type.is_ref || ret_type.is_pointer {
        let len = type_name.len();
        // "remove" pointer or ref from the name
        if type_name.as_bytes()[len - 2] == b' ' {
            type_name = &type_name[..len - 2];
        } else {
            type_name = &type_name[..len - 1];
        }
    }

    if let Some(enum_name) = type_name.rfind("::Enum") {
        ret_type.var_type = VarType::Enum(type_name[..enum_name].to_owned());
        return ret_type;
    }

    // TODO: Cleanup, check if primitive again

    match type_name {
        e @ "bool" => ret_type.var_type = VarType::Primitive(e.to_owned()),
        e @ "char" => ret_type.var_type = VarType::Primitive(e.to_owned()),
        e @ "float" => ret_type.var_type = VarType::Primitive(e.to_owned()),
        e @ "int8_t" => ret_type.var_type = VarType::Primitive(e.to_owned()),
        e @ "int16_t" => ret_type.var_type = VarType::Primitive(e.to_owned()),
        e @ "int32_t" => ret_type.var_type = VarType::Primitive(e.to_owned()),
        e @ "int64_t" => ret_type.var_type = VarType::Primitive(e.to_owned()),
        e @ "uint8_t" => ret_type.var_type = VarType::Primitive(e.to_owned()),
        e @ "uint16_t" => ret_type.var_type = VarType::Primitive(e.to_owned()),
        e @ "uint32_t" => ret_type.var_type = VarType::Primitive(e.to_owned()),
        e @ "uint64_t" => ret_type.var_type = VarType::Primitive(e.to_owned()),
        e @ "void" => ret_type.var_type = VarType::Primitive(e.to_owned()),
        _ => (),
    }

    let dummy = String::new();

    if ret_type.var_type == VarType::Unknown(dummy) {
        // we assume it's a struct if we are here
        ret_type.var_type = VarType::Struct(type_name.to_owned());
    }

    ret_type
}

// Parse a function/struct. A function can be of the following formats:
//
// func.<StructName>.<funcname>
// "return value"
// .param "type" { extra data}
//
// func.<funcname>
// "return value"
//
fn parse_func_or_struct(
    in_func: &FunctionCall,
    gather_com: &mut GatherComments,
    skip_class_ret: bool,
) -> Func {
    enum State {
        ReturnType,
        ArgName,
        ArgType,
        MaybeTable,
    }

    let mut func = Func::default();
    let mut iter = in_func.suffixes();

    let name_or_class = get_dot_name(iter.next().unwrap());
    let mut it = iter.next().unwrap();
    let name = get_dot_name(it);

    // If we found an extension name we need to advance the iterator, otherwise assume the current
    // iterator is the return type or attributes for the function
    if !name.text.is_empty() && !skip_class_ret {
        func.class = name_or_class;
        func.name = name;
        it = iter.next().unwrap();
    } else {
        func.name = name_or_class;
        func.class.text.clear();
    }

    // Get table/attribute data for the function
    if let Some(table) = get_table(it) {
        func.table = table;
    }

    // if we found a table we need to advance the iterator again
    if !func.table.is_empty() {
        it = iter.next().unwrap();
    }

    let mut arg = FuncArg::default();
    let mut state = if skip_class_ret {
        State::ArgName
    } else {
        State::ReturnType
    };

    loop {
        match state {
            State::ReturnType => {
                func.return_name_line = get_arg_string(it);
                func.return_type = get_detailed_type(&func.return_name_line.text, false);
                state = State::ArgName;
            }

            State::ArgName => {
                arg.name_line = get_dot_name(it);

                // fix reserved rust names
                if arg.name_line.text == "enum" || arg.name_line.text == "type" {
                    arg.name_line.text.push_str("_r");
                }

                state = State::ArgType;
            }

            State::ArgType => {
                let arg_type = get_arg_string(it);
                arg.type_name = arg_type.text;
                state = State::MaybeTable;
            }

            State::MaybeTable => {
                let had_table = if let Some(table) = get_table(it) {
                    arg.table = table;
                    true
                } else {
                    false
                };

                //arg.table = get_table(it);
                //let had_table = arg.table.is_some();

                func.args.push(arg);
                arg = FuncArg::default();

                if had_table {
                    state = State::ArgName;
                } else {
                    // if this wasn't a table its the first arg of the next argument.
                    arg.name_line = get_dot_name(it);

                    // fix reserved rust names
                    if arg.name_line.text == "enum" || arg.name_line.text == "type" {
                        arg.name_line.text.push_str("_r");
                    }

                    state = State::ArgType;
                }
            }
        }

        match iter.next() {
            Some(val) => it = val,
            None => break,
        };
    }

    if !arg.name_line.text.is_empty() {
        func.args.push(arg);
    }

    // update arg types

    for arg in &mut func.args {
        let is_output = arg
            .table
            .iter()
            .find(|t| t.name == "out" || t.name == "inout")
            .is_some();

        arg.arg_type = get_detailed_type(&arg.type_name, is_output);
    }

    // Update comments
    update_comments(&mut func, gather_com);

    func
}

impl<'ast> Visitor<'ast> for GatherComments {
    fn visit_single_line_comment(&mut self, token: &Token<'ast>) {
        let text = match token.token_type() {
            TokenType::SingleLineComment { comment } => comment,
            _ => panic!(),
        };

        let pos = token.start_position();

        if let Some(prefix) = text.strip_prefix("-") {
            self.comments[pos.line()] = Some(Comment {
                pos: pos.character(),
                text: prefix.to_owned(),
            });
        }
    }
}

/// Parse the bgfx idl and return the parsed data
pub fn parse_bgfx_idl(filename: &str) -> Result<Idl, Box<dyn std::error::Error + 'static>> {
    let bgfx_defs: String = fs::read_to_string(filename)?;
    let line_count = bgfx_defs.as_bytes().iter().filter(|&&c| c == b'\n').count();
    let lua_data = full_moon::parse(&bgfx_defs).unwrap();

    let mut coms = GatherComments {
        comments: vec![None; line_count],
        codelines: vec![false; line_count],
    };

    coms.visit_ast(&lua_data);

    let mut i = Idl::default();

    for node in lua_data.nodes().stmts() {
        if let Stmt::FunctionCall(func) = node {
            match get_prefix_identifier(func.prefix()) {
                "version" => i.version = get_version(func),
                "typedef" => i.typedefs.push(parse_typedef_handle(func, &mut coms)),
                "handle" => i.handles.push(parse_typedef_handle(func, &mut coms)),
                "flag" => i.flags.push(parse_enum_or_flag(func, &mut coms, true)),
                "enum" => i.enums.push(parse_enum_or_flag(func, &mut coms, false)),
                "struct" => i.structs.push(parse_func_or_struct(func, &mut coms, true)),
                "func" => i.funcs.push(parse_func_or_struct(func, &mut coms, false)),
                _ => (),
            }
        }
    }

    Ok(i)
}
