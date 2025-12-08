use std::{collections::HashMap, path::PathBuf, sync::Arc};

use color_eyre::{eyre::Result, owo_colors::OwoColorize};

use crate::{
    checker::TypeChecker,
    compiler::Compiler,
    hir::{SlynxHir, macros::js::JSMacro},
    intermediate::IntermediateRepr,
    parser::{
        Parser,
        error::ParseError,
        lexer::{Lexer, error::LexerError},
    },
};

#[derive(Debug)]
///The type of the error that was generated
pub enum SlynxErrorType {
    Lexer,
    Parser,
    Hir,
    Type,
    Compilation,
}

#[derive(Debug)]
///An error that will be shown if something fails
pub struct SlynxError {
    ty: SlynxErrorType,
    line: usize,
    column_start: usize,
    column_end: usize,
    message: String,
    ///The file path the error occuried
    file: String,
    source: String,
}
impl std::error::Error for SlynxError {}

impl std::fmt::Display for SlynxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let type_error = match self.ty {
            SlynxErrorType::Lexer => "Lexing Error",
            SlynxErrorType::Parser => "Parsing Error",
            SlynxErrorType::Hir => "Name Resolution Error",
            SlynxErrorType::Compilation => "Compilation Error",
            SlynxErrorType::Type => "Type Checking Error",
        };
        let source = format!("{} | {}", self.line, self.source);
        let mut err = " ".repeat((self.column_end * 2).min(self.source.len()));

        err.replace_range(
            self.column_start - 1..err.len(),
            &"^".repeat(err.len() - (self.column_start - 1)),
        );
        let err = format!("  | {}", err);
        writeln!(
            f,
            "{}: {} => {}:{}:{}{}",
            type_error.green().bold(),
            self.message.red(),
            self.file.bold(),
            self.line.blue().bold(),
            self.column_start.blue().bold(),
            format!("\n{source}\n{err}")
        )
    }
}

///Context that will have all the information needed when erroring or retrieving metadata about the code itself during compilation.
///For example, this can be used when erroring to retrieve the correct line where the file errored
pub struct SlynxContext {
    ///The source code of the files. Maps the name of some to it's source code. Can and is used when importing contents(will be implemented yet)
    files: HashMap<Arc<PathBuf>, String>,
    ///Maps the name of some file to it's lines. Used when wanting to retrieve for example, returning the lines where an error occuried
    lines: HashMap<Arc<PathBuf>, Vec<usize>>,
    entry_point: Arc<PathBuf>,
}

impl SlynxContext {
    pub fn new(entry_point: Arc<PathBuf>) -> Result<Self> {
        let mut out = Self {
            files: HashMap::new(),
            lines: HashMap::new(),
            entry_point: entry_point.clone(),
        };
        out.insert_file(entry_point)?;
        Ok(out)
    }

    ///Gets the source code of the file that will start all the compilation
    pub fn get_entry_point_source(&self) -> &str {
        self.files
            .get(&self.entry_point)
            .expect("Entry point should map to a file")
    }

    ///Inserts the file with provided `path` if it exists.
    pub fn insert_file(&mut self, path: Arc<PathBuf>) -> Result<()> {
        let file = std::fs::read_to_string(&path.as_path())?;
        let lines = file
            .chars()
            .enumerate()
            .filter_map(|(idx, c)| if c == '\n' { Some(idx) } else { None })
            .collect::<Vec<_>>();

        self.files.insert(path.clone(), file);
        self.lines.insert(path, lines);
        Ok(())
    }

    ///Based on the provided `index`, which is the index of a char on the source code of `path`, returns the line where it's located on the file of the provided `path`.
    ///This will return its line and the column and the line containing the error
    pub fn get_line_info(&self, path: &Arc<PathBuf>, index: usize) -> (usize, usize, &str) {
        let lines = self
            .lines
            .get(path)
            .expect("Path should be provided on the context");
        let source = self
            .files
            .get(path)
            .expect("Path should be provided on the context");
        let out = match lines.binary_search(&index) {
            Ok(v) => (
                v,
                index - lines[v],
                &source[lines[index - lines[v]]..lines[v + 1]],
            ),
            Err(e) => (
                e + 1,
                {
                    let mut column = index.saturating_sub(lines[e.saturating_sub(1)]);
                    if column == 0 {
                        column = index + 1;
                    }
                    column
                },
                &source[lines[e - 1] + 1..lines[e]],
            ),
        };

        out
    }

    ///The name of the file this context is parsing
    pub fn file_name(&self) -> String {
        self.entry_point.to_string_lossy().to_string()
    }

    pub fn start_compilation(self) -> Result<()> {
        let stream = match Lexer::tokenize(self.get_entry_point_source()) {
            Ok(value) => value,
            Err(e) => match e {
                LexerError::UnrecognizedChar { index, .. } => {
                    let (line, column, src) = self.get_line_info(&self.entry_point, index);
                    return Err(SlynxError {
                        line,
                        ty: SlynxErrorType::Lexer,
                        column_start: column,
                        column_end: column,
                        message: e.to_string(),
                        file: self.entry_point.to_string_lossy().to_string(),
                        source: src.to_string(),
                    }
                    .into());
                }
            },
        };
        let decls = match Parser::new(stream).parse_declarations() {
            Ok(v) => v,
            Err(e) => {
                return match e {
                    ParseError::UnexpectedToken(ref token, _) => {
                        let (line, column, src) =
                            self.get_line_info(&self.entry_point, token.span.start);
                        Err(SlynxError {
                            line,
                            ty: SlynxErrorType::Parser,
                            column_start: column,
                            column_end: column + (token.span.end - token.span.start),
                            message: e.to_string(),
                            file: self.file_name(),
                            source: src.to_string(),
                        }
                        .into())
                    }
                    ParseError::UnexpectedEndOfInput => {
                        let (line, column, src) = self.get_line_info(
                            &self.entry_point,
                            self.lines.get(&self.entry_point).unwrap().len() - 1,
                        );
                        Err(SlynxError {
                            line,
                            ty: SlynxErrorType::Parser,
                            column_start: column,
                            column_end: column,
                            message: e.to_string(),
                            file: self.file_name(),
                            source: src.to_string(),
                        }
                        .into())
                    }
                };
            }
        };
        let mut hir = SlynxHir::new();

        {
            let jsmacro = Arc::new(JSMacro {});
            hir.insert_element_macro(jsmacro.clone());
            hir.insert_statment_macro(jsmacro);
        }

        if let Err(e) = hir.generate(decls) {
            let (line, column, src) = self.get_line_info(&self.entry_point, e.span.start);
            return Err(SlynxError {
                line,
                column_start: column,
                column_end: column + (e.span.end - e.span.start),
                ty: SlynxErrorType::Hir,
                message: e.to_string(),
                file: self.entry_point.to_string_lossy().to_string(),
                source: src.to_string(),
            }
            .into());
        }
        if let Err(e) = TypeChecker::check(&mut hir) {
            let (line, column, src) = self.get_line_info(&self.entry_point, e.span.start);
            return Err(SlynxError {
                line,
                column_start: column,
                column_end: column + (e.span.end - e.span.start),
                ty: SlynxErrorType::Type,
                message: e.to_string(),
                file: self.file_name(),
                source: src.to_string(),
            }
            .into());
        };
        let mut ir = IntermediateRepr::new();
        ir.generate(hir.declarations);

        let out = Compiler::new().compile(&ir);
        std::fs::write(self.entry_point.with_extension("js"), out)?;
        Ok(())
    }
}
