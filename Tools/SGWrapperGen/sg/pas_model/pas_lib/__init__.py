from converter_helper import load_templates, get_template

"""
The indenter stores the number of indents a particular structure has from the current 'base'
"""
indenter = {
    "statement"                 : (+1),
    "block_compound_statement"  : (+1),
    "variable"                  : (+1),
    "types"                     : (+1),
    "record_field"              : (+1),
    "cases"                     : (+1),
            }

_val_switcher = {

}

_data_switcher = {

}

_type_switcher = {

}

_adapter_type_switcher = {

}

_operator_conversion_table = {

    }     

_type_dicts = {
    '_type_switcher': _type_switcher, 
    '_adapter_type_switcher': _adapter_type_switcher,
    }

def convert_array_declaration(array, is_parameter):
    """
    converts an array to a string describing the array
    """
    from converter_helper import lower_name, convert_type
    var_name = array.name
    type_name = convert_type(_type_switcher, array.type.nested_type, None)

    if is_parameter:
        return '%s : array of %s' % (var_name, type_name ) 
    else:
        str_dimensions = '['
        for dimension in array.type.dimensions:
            str_dimensions += '%s..%s' % (dimension[0], dimension[1])
        str_dimensions += ']'
        return '%s : array %s of %s;\n' % (var_name, str_dimensions, type_name ) 

extension = '.pas'

post_proc = None

proper_name = "Pascal"

statement_seperator = ';'

load_templates("pas_lib/", ".pas")
# templates must be added to this list otherwise they will be unavailable
variable_decl_template              = get_template("variable_declaration.pas")
variable_template                   = get_template("variable.pas")
expression_template                 = get_template("expression.pas")
string_template                     = get_template("string.pas")
function_call_template              = get_template("function_call.pas")
assignment_template                 = get_template("assignment_statement.pas")
argument_template                   = get_template("arguments.pas")
identifier_template                 = get_template("identifier.pas")
compound_statement_template         = get_template("compound_statement.pas")
while_statement_template            = get_template("while.pas")
parameter_template                  = get_template("parameters.pas")
if_statement_template               = get_template("if.pas")
block_template                      = get_template("block.pas")
function_declaration_template       = get_template("function_declaration.pas")
procedure_declaration_template      = get_template("procedure_declaration.pas")
program_template                    = get_template("program.pas")
inner_expression_template           = get_template("inner_expression.pas")
comment_template                    = get_template("comment.pas")
type_declaration_template           = get_template("type_declaration.pas")
record_template                     = get_template("record.pas")
record_field_template               = get_template("record_field.pas")
enum_value_template                 = get_template("enum_values.pas")
enum_template                       = get_template("enum.pas")
unit_reference_template             = get_template("unit_reference.pas")
uses_clause_template                = get_template("uses_clause.pas")
repeat_statement_template           = get_template("repeat_statement.pas")
else_statement_template             = get_template("else.pas")
block_compound_statement_template   = get_template('block_compound_statement.pas')
function_call_expr_template         = get_template('function_call_expr.pas')
case_statement_template             = get_template('case_statement.pas')
case_template                       = get_template('case.pas')
var_pointer_dereference_template    = get_template('var_pointer_dereference.pas')
var_record_dereference_template     = get_template('var_record_dereference.pas')
var_array_dereference_template      = get_template('var_array_dereference.pas')