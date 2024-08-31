import ply.lex as lex
import ply.yacc as pars

# Define token types
tokens = (
    'KEYWORD',
    'IDENTIFIER',
    'NUMBER',
    'STRING',
    'OPERATOR',
    'PUNCTUATION',
    'BOOLEAN',
    'NULL',
    'ARRAY_START',
    'ARRAY_END',
    'OBJECT_START',
    'OBJECT_END',
    'PROPERTY_ACCESS',
    'METHOD_CALL',
    'ASSIGNMENT',
    'VIEW_START',
    'VIEW_END',
    'ELEMENT_DECLARATION',
    'PROPERTY_NAME',
    'PROPERTY_VALUE',
    'EXTENSION_KEYWORD',
    'ELEMENT_TYPE',
    'EVENT_KEYWORD',
    'EVENT_TARGET',
    'ARROW_OPERATOR',
    'FUNCTION_CALL',
)

# Regular expressions for tokens
t_KEYWORD = r'(if|else|for|while|function|var|int|double|String|bool)'
t_IDENTIFIER = r'[a-zA-Z_][a-zA-Z0-9_]*'
t_NUMBER = r'\d+'
t_STRING = r'"[^"]*"|'
t_OPERATOR = r'[\+\-\*/<>=!&\|]'
t_PUNCTUATION = r'[(),{}\[\];:]'
t_BOOLEAN = r'(true|false)'
t_NULL = r'null'
t_ARRAY_START = r'\['
t_ARRAY_END = r'\]'
t_OBJECT_START = r'{'
t_OBJECT_END = r'}'
t_PROPERTY_ACCESS = r'\.'
t_METHOD_CALL = r'\('
t_ASSIGNMENT = r'(=|\+=|-=|\*=|/=|%=|&=|\|=|\^=|\<<=|\>>=|\>>>=)'
t_VIEW_START = r'newView'
t_VIEW_END = r'\}'
t_ELEMENT_DECLARATION = r'@'
t_PROPERTY_NAME = r'[a-zA-Z\-]+'  # Matches property names with hyphens
t_PROPERTY_VALUE = r'[^;}]+'  # Matches values until a semicolon or closing brace
t_EXTENSION_KEYWORD = r'ext'
t_ELEMENT_TYPE = r'(Text|Image|Element)'
t_EVENT_KEYWORD = r'onClick'
t_EVENT_TARGET = r'for @'
t_ARROW_OPERATOR = r'=>'
t_FUNCTION_CALL = r'[a-zA-Z_][a-zA-Z0-9_]*\('

# Ignore whitespace and comments
t_ignore = ' \t\n'
t_ignore_COMMENT = r'//.*'

# Error handling
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# Build lexer
lexer = lex.lex()

# Parsing rules
def p_program():
    """program : statement*"""
    # Parse the program and generate code if needed
    print("Parsed program successfully.")

def p_statement_expression(p):
    """statement : expression"""
    # Evaluate the expression
    print("Evaluated expression:", p[1])

def p_expression_binary_op(p):
    """expression : expression OPERATOR expression"""
    # Perform arithmetic operation
    if p[2] == '+':
        p[0] = p[1] + p[3]
    elif p[2] == '-':
        p[0] = p[1] - p[3]
    # ... other operators
    print("Performed arithmetic operation:", p[0])

def p_statement_assignment(p):
    """statement : IDENTIFIER '=' expression"""
    # Assign the expression to the variable
    print("Assigned value to variable:", p[1], "=", p[3])

def p_expression_conditional(p):
    """expression : conditional_expression ('?' expression ':' expression)?"""
    if p[3] is not None:
        p[0] = p[2] if p[1] else p[4]
    else:
        p[0] = p[1]

def p_conditional_expression_logical_or(p):
    """conditional_expression : logical_or_expression ('||' logical_and_expression)*"""
    p[0] = p[1]
    for i in range(3, len(p), 2):
        p[0] = p[0] or p[i]

def p_logical_and_expression(p):
    """logical_and_expression : equality_expression ('&&' equality_expression)*"""
    p[0] = p[1]
    for i in range(3, len(p), 2):
        p[0] = p[0] and p[i]

def p_equality_expression(p):
    """equality_expression : relational_expression ('==' | '!=' equality_expression)*"""
    p[0] = p[1]
    for i in range(3, len(p), 2):
        if p[2] == '==':
            p[0] = p[0] == p[i]
        elif p[2] == '!=':
            p[0] = p[0] != p[i]

def p_relational_expression(p):
    """relational_expression : additive_expression ('>' | '<' | '>=' | '<=' relational_expression)*"""
    p[0] = p[1]
    for i in range(3, len(p), 2):
        if p[2] == '>':
            p[0] = p[0] > p[i]
        elif p[2] == '<':
            p[0] = p[0] < p[i]
        elif p[2] == '>=':
            p[0] = p[0] >= p[i]
        elif p[2] == '<=':
            p[0] = p[0] <= p[i]

def p_additive_expression(p):
    """additive_expression : multiplicative_expression ('+' | '-' additive_expression)*"""
    p[0] = p[1]
    for i in range(3, len(p), 2):
        if p[2] == '+':
            p[0] = p[0] + p[i]
        elif p[2] == '-':
            p[0] = p[0] - p[i]

def p_multiplicative_expression(p):
    """multiplicative_expression : primary_expression ('*' | '/' | '%' multiplicative_expression)*"""
    p[0] = p[1]
    for i in range(3, len(p), 2):
        if p[2] == '*':
            p[0] = p[0] * p[i]
        elif p[2] == '/':
            p[0] = p[0] / p[i]
        elif p[2] == '%':
            p[0] = p[0] % p[i]

def p_primary_expression_identifier(p):
    """primary_expression : IDENTIFIER"""
    p[0] = p[1]

def p_primary_expression_number(p):
    """primary_expression : NUMBER"""
    p[0] = int(p[1])

def p_primary_expression_string(p):
    """primary_expression : STRING"""
    p[0] = p[1][1:-1]

def p_primary_expression_boolean(p):
    """primary_expression : BOOLEAN"""
    p[0] = p[1] == 'true'

def p_primary_expression_null(p):
    """primary_expression : NULL"""
    p[0] = None

def p_primary_expression_parentheses(p):
    """primary_expression : '(' expression ')'"""
    p[0] = p[2]

def p_primary_expression_array_literal(p):
    """primary_expression : array_literal"""
    p[0] = p[1]

def p_array_literal(p):
    """array_literal : '[' element_list ']'"""
    p[0] = []
    if len(p) > 2:
        for i in range(1, len(p), 2):
            p[0].append(p[i])

def p_primary_expression_object_literal(p):
    """primary_expression : object_literal"""
    p[0] = p[1]

def p_object_literal(p):
    """object_literal : '{' property_list '}'"""
    p[0] = {}

parser = pars.yacc()
Lexer = lex.yacc()