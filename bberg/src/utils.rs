/// Get Relations Imports
///
/// We may have multiple relation files in the generated foler
/// This method will return all of the imports for the relation header files
pub fn get_relations_imports(name: &str, relations: &[String]) -> String {
    let transformation = |relation_name: &_| format!(
        "#include \"barretenberg/relations/generated/{name}/{relation_name}.hpp\""
    );

    map_with_newline(relations, transformation)
}

/// Sanitize Names
///
/// Column titles that we get from pil contain . to distinguish which pil namespace they belong to
/// We need to replace these with _ to make them valid C++ identifiers
pub fn sanitize_name(string: &String) -> String {
    string.replace(".", "_").replace("[", "_").replace("]", "_")
}

/// Capitalize
pub fn capitalize(s: &String) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

/// Map With Newline 
/// This utility function is used all over the codegen pipeline
/// It takes a list, usually the names of columns in an execution trace and applies a string transformation "op"
/// to each element in the list
pub fn map_with_newline<F>(list: &[String], op: F) -> String 
where F: Fn(&String) -> String 
{
    list.iter().map(op).collect::<Vec<String>>().join("\n")
}