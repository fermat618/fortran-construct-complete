#!/usr/bin/python3

__all__ = ['do_complete_fortran_statement', 'complete_fortran_statement']

import vim
import re
from functools import reduce
from itertools import permutations
import operator
import pyparsing as p
from pyparsing import Word, Forward, Group, Optional, oneOf, ZeroOrMore, nums, \
        alphas, alphanums, delimitedList, originalTextFor, ParseBaseException, \
        Literal, quotedString, Keyword, empty, Suppress, Combine, NotAny, Regex


def eachMostOnce(*args, or_=operator.ior, and_=operator.add):
    return reduce(or_,
                  (reduce(or_,
                          map(lambda x: reduce(and_, x), permutations(args, i)))
                   for i in range(len(args), 0, -1)))


NAME = Word(alphas, alphanums + '_')
INTEGER = Word(nums).setName('integer')
INTEGER_K = Combine(INTEGER + Optional('_' + (INTEGER | NAME)))
EOL = p.LineEnd()
FortranComment = Regex(r'!.*$')
FortranComment.setParseAction(lambda s,loc,toks: [' '+toks[0]])
EOLL = Optional(FortranComment) + EOL
precision = Combine('.' + INTEGER)
exponent = Combine(oneOf('d e D E') + Optional(oneOf('+ -')) +  INTEGER)
REAL = Combine(INTEGER + ((precision + exponent) | precision | exponent))
STRING = quotedString
comp_op = Forward()
user_op = NotAny(comp_op | oneOf('.not. .and. .or. .eqv. .neqv. ** // % .true. .false.')) \
        + Combine('.' + NAME + '.')

atom = Forward()

calllist = Forward()
array_sub = '(' + Optional(atom)+':'+Optional(atom) + Optional(':'+atom) + ')'
type_sub = '%' + NAME
trailer = p.Or((calllist, array_sub, type_sub))

user_monadic_expr = Forward()
user_monadic_expr << ((user_op + user_monadic_expr) | atom)
factor = Forward()
power = user_monadic_expr +  ZeroOrMore(trailer) + Optional('**' + factor)
factor << power
term = factor + ZeroOrMore(oneOf('* /') + factor)
pm_term = Forward()
pm_term << ((oneOf('+ -') + pm_term) | term)
arith_expr = pm_term + ZeroOrMore(oneOf('+ -') + pm_term)
cated_expr = arith_expr + ZeroOrMore('//' + arith_expr)
comp_op << oneOf('< > == >= <= /= .lt. .gt. .eq. .ge. .le. .ne.')
comparison = arith_expr + ZeroOrMore(comp_op + arith_expr)
not_test = Forward()
not_test << (('.not.' + not_test) | comparison)
and_test = not_test + ZeroOrMore('.and.' + not_test)
or_test = and_test + ZeroOrMore('.or.' + and_test)
logical_eq_test = or_test + ZeroOrMore(oneOf('.eqv. .neqv.') + or_test)
user_dyadic_test = logical_eq_test + ZeroOrMore(user_op + logical_eq_test)
test = user_dyadic_test

calllist << ('(' + Optional(delimitedList(test, delim=',')) + ')')
array_literal = '(/' + delimitedList(test, delim=',') + '/)'
atom << (('(' + test + ')') | array_literal | NAME | INTEGER_K | REAL | STRING | oneOf('.true. .false.'))

orig_test = originalTextFor(test)
orig_test.addParseAction(lambda s,loc,toks: [toks[0].strip()])


funcall = Group(NAME + '(' + delimitedList(test, delim=',') + ')')

comma = Literal(',').setParseAction(lambda s,loc,toks: [', '])

do_kwd = p.Keyword('do').setParseAction(lambda s,loc,toks: ['do '])
ivar = NAME.setResultsName('ivar')
istart = orig_test.setResultsName('istart')
eqsign = p.Literal('=').setParseAction(lambda s,loc,toks: [' = '])
comma = p.Literal(',').setParseAction(lambda s,loc,toks: [', '])
iend = orig_test.setResultsName('iend')
istep = orig_test.setResultsName('istep')

do_stmt = do_kwd + ivar + eqsign + istart + comma + iend + Optional(comma + istep)\
        + EOLL
do_stmt.setParseAction(lambda s,loc,toks: [''.join(toks), '', 'enddo'])

if_expr = (Suppress('(') + orig_test + Suppress(')')) | orig_test
if_expr.setParseAction(lambda s,loc,toks: [' (' + ''.join(toks) + ')' ])
if_expr_2 = if_expr.copy()
if_expr_2.setParseAction(lambda s,loc,toks: [' (' + ''.join(toks) + ')' + ' then' ])
if_stmt = Keyword('if') + if_expr_2 + Optional(Suppress('then')) + EOLL
if_stmt.setParseAction(lambda s,loc,toks: [''.join(toks), '', 'endif'])

elseif_stmt = Keyword('elseif') + if_expr_2 + Optional(Suppress('then')) + EOLL
elseif_stmt.setParseAction(lambda s,loc,toks: [''.join(toks), ''])

while_kwd = Keyword('while').setParseAction(lambda s,loc,toks: [' while'])
dowhile_stmt = Keyword('do') + while_kwd + if_expr + EOLL
dowhile_stmt.setParseAction(lambda s,loc,toks: [''.join(toks), '', 'enddo'])

selectcase_kwd = Keyword('select') + Keyword('case')
selectcase_kwd.setParseAction(lambda s,loc,toks: [' '.join(toks)])
selectcase_stmt = selectcase_kwd + if_expr + EOLL
selectcase_stmt.setParseAction(lambda s,loc,toks: [''.join(toks), '', 'end select'])

where_stmt = Keyword('where') + if_expr + EOLL
where_stmt.setParseAction(lambda s,loc,toks: [''.join(toks), '', 'end where'])

arglist = '(' + (delimitedList(NAME, delim=',') | empty) + ')'
arglist.setParseAction(lambda s,loc,toks: '('+', '.join(toks[1:-1]) + ')')
arglist.parseString('(foo,bar)')

name_eq = Literal('name') + '=' + quotedString
bind_keyword = Keyword('bind').setParseAction(lambda s,loc,toks: [' bind'])
bind_attr = bind_keyword + Literal('(') + 'c' + Optional(comma + name_eq) + ')'
result_attr = Literal('result') + '(' + NAME + ')'
result_attr.setParseAction(lambda s,loc,toks: [' '+''.join(toks)])
func_post = eachMostOnce(bind_attr, result_attr)

elem_attr = Keyword('elemental').setName('elemental')
elem_attr.setParseAction(lambda s,loc,toks: [toks[0] + ' '])
pure_attr = Keyword('pure').setName('pure')
pure_attr.setParseAction(lambda s,loc,toks: [toks[0] + ' '])
recu_attr = Keyword('recursive').setName('elemental')
recu_attr.setParseAction(lambda s,loc,toks: [toks[0] + ' '])
func_pre  = eachMostOnce(elem_attr, pure_attr, recu_attr)

func_name = NAME.copy().setName('func_name')
func_name.setParseAction(lambda s,loc,toks: [' '+toks[0]])


func_def = Optional(func_pre) + Keyword('function') + func_name + arglist \
        + Optional(func_post) + EOLL
func_def.setParseAction(lambda s,loc,toks: [''.join(toks), '', 'end function'])

subr_def = Keyword('subroutine') + func_name + arglist + Optional(bind_attr) + EOLL
subr_def.setParseAction(lambda s,loc,toks: [''.join(toks), '', 'end subroutine'])

inte_name = NAME.copy().setParseAction(lambda s,loc,toks: [' '+toks[0]])
inte_assignment = Literal('assignment') + '(' + Literal('=') + ')'
inte_assignment.setParseAction(lambda s,loc,toks: [' ' + ''.join(toks)])
operators = comp_op | oneOf('+ - * / ** //') | Combine('.' + NAME + '.')
inte_operator = Literal('operator') + '(' + operators + ')'
inte_operator.setParseAction(lambda s,loc,toks: [' ' + ''.join(toks)])
inte_stmt = Keyword('interface') + Optional(inte_assignment | inte_operator | inte_name)\
        + EOLL
inte_stmt.setParseAction(lambda s,loc,toks: [''.join(toks), '', 'end interface'])

prog_name = NAME.copy().setParseAction(lambda s,loc,toks: [' '+toks[0]])
prog_stmt = Keyword('program') + prog_name + EOLL
prog_stmt.setParseAction(lambda s,loc,toks: [''.join(toks), '', 'end program'])

modu_name = NAME.copy().setParseAction(lambda s,loc,toks: [' '+toks[0]])
modu_stmt = Keyword('module') + modu_name + EOLL
modu_stmt.setParseAction(lambda s,loc,toks: [''.join(toks), '', 'end module'])


type_name = NAME.copy().setName('type_name')
type_name.setParseAction(lambda s,loc,toks: [' '+toks[0]])
diamon = Literal('::')
diamon.setParseAction(lambda s,loc,toks: [' '+toks[0]])
type_def = Keyword('type') + \
        Optional(Optional(eachMostOnce(
            comma + (Literal('private') | Literal('public')) , ',' + bind_attr)) + diamon) \
        + type_name + Optional(arglist)
type_def.setParseAction(lambda s,loc,toks: [''.join(toks), '', 'end type'])

association = NAME + Literal('=>') + orig_test
association.setParseAction(lambda s,loc,toks: [' '.join(toks)])
asso_list0 = association + ZeroOrMore(comma + association)
asso_list = (Suppress('(') + asso_list0 + Suppress(')')) | asso_list0
asso_list.setParseAction(lambda s,loc,toks: [' (' + ''.join(toks) + ')' ])
asso_stmt = Keyword('associate') + asso_list + EOLL
asso_stmt.setParseAction(lambda s,loc,toks: [''.join(toks), '', 'end associate'])

forall_range = originalTextFor(test + ':' + test + Optional(':' + test))
forall_range.addParseAction(lambda s,loc,toks: [toks[0].strip()])
forall_indexstep = NAME + eqsign + forall_range
forall_list0 = forall_indexstep + ZeroOrMore(comma + forall_indexstep)\
        + Optional(comma + orig_test)
forall_list = (Suppress('(') + forall_list0 + Suppress(')')) | forall_list0
forall_list.setParseAction(lambda s,loc,toks: [' (' + ''.join(toks) + ')' ])
forall_stmt = Keyword('forall') + forall_list + EOLL
forall_stmt.setParseAction(lambda s,loc,toks: [''.join(toks), '', 'end forall'])

stmts = do_stmt | dowhile_stmt | if_stmt | elseif_stmt| selectcase_stmt | where_stmt |  func_def | \
        subr_def | inte_stmt | prog_stmt | modu_stmt | type_def | asso_stmt | forall_stmt

def complete_fortran_statement(str, shiftwidth=4):
    ind = re.match(r'^ *', str).end()
    try:
        parsed = stmts.parseString(str)
    except ParseBaseException:
        return
    return [(' '*ind + line if len(line) != 0 else ' '*(shiftwidth + ind))
            for line in parsed]

def do_complete_fortran_statement():
    shiftwide = int(vim.eval('&l:shiftwidth'))
    if shiftwide < 1:
        shiftwide = 4
    sline = vim.current.line
    lineno = vim.current.window.cursor[0]
    if vim.current.window.cursor[1] + 1 <= len(vim.current.line):
        return False

    out = complete_fortran_statement(sline.rstrip(), shiftwide)
    if out is None:
        return False

    vim.current.buffer[lineno-1] = out[0]
    vim.current.buffer[lineno:lineno] = out[1:]
    lineadd = 1
    col = len(out[1])
    for (i, line) in enumerate(out):
        if re.match(r'^\s*$', line):
            lineadd = i
            col = len(line)
            break
    vim.current.window.cursor = (lineno+lineadd, col)
    return True

