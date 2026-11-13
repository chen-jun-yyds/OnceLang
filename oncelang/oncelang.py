import sys
import re
import struct
from pathlib import Path

# 默认关闭 DEBUG
DEBUG = False 

def debug(msg):
    if DEBUG:
        print(f"[DEBUG] {msg}")

# ==================== 词法分析器 (Lexer) ====================
class Token:
    def __init__(self, type, value, start, end):
        self.type = type
        self.value = value
        self.start = start
        self.end = end
    def __repr__(self):
        return f"Token({self.type}, {self.value})"

def lex(source):
    tokens = []
    token_spec = [
        # 核心修复：HEX 必须在 NUMBER 前面
        ('HEX',      r'0x[0-9a-fA-F]+'),
        ('NUMBER',   r'\d+'),
        ('STRING',   r'"[^"]*"'),
        ('ID',       r'[a-zA-Z_][a-zA-Z0-9_]*'),
        ('COMMENT',  r'//.*'),
        ('OP',       r'[=(),+\-*/{}<>;\[\]]'),
        ('SKIP',     r'[ \t\n]+'),
        ('MISMATCH', r'.'),
    ]
    tok_regex = '|'.join('(?P<%s>%s)' % pair for pair in token_spec)
    for mo in re.finditer(tok_regex, source):
        kind = mo.lastgroup
        value = mo.group()
        start, end = mo.span()
        if kind in ['SKIP', 'COMMENT']:
            continue
        tokens.append(Token(kind, value, start, end))
    tokens.append(Token('EOF', '', len(source), len(source)))
    return tokens

# ==================== 语法解析器 (Parser) ====================
class Node: pass
class Literal(Node):
    def __init__(self, val): self.val = val
class Var(Node):
    def __init__(self, name): self.name = name
class Index(Node):
    def __init__(self, base, idx): self.base = base; self.idx = idx
class InterpCall(Node):
    def __init__(self, interp, args): self.interp = interp; self.args = args
class BinOp(Node):
    def __init__(self, left, op, right): self.left = left; self.op = op; self.right = right
class Print(Node):
    def __init__(self, expr): self.expr = expr
class Assign(Node):
    def __init__(self, targets, exprs): self.targets = targets; self.exprs = exprs
class Return(Node):
    def __init__(self, expr): self.expr = expr
class Block(Node):
    def __init__(self, stmts): self.stmts = stmts
class InterpDef(Node):
    def __init__(self, name, params, body): self.name = name; self.params = params; self.body = body
class While(Node):
    def __init__(self, cond, body): self.cond = cond; self.body = body
class If(Node):
    def __init__(self, cond, body): self.cond = cond; self.body = body
class For(Node):
    def __init__(self, init, cond, step, body): self.init = init; self.cond = cond; self.step = step; self.body = body
class ErrorNode(Node):
    def __init__(self, text): self.text = text

class Parser:
    def __init__(self, tokens, source):
        self.tokens = tokens
        self.source = source
        self.pos = 0

    def peek(self):
        if self.pos >= len(self.tokens):
            return self.tokens[-1]
        return self.tokens[self.pos]

    def next(self):
        t = self.peek()
        if self.pos < len(self.tokens) - 1:
            self.pos += 1
        return t

    def expect(self, type, val=None):
        t = self.next()
        if t.type != type or (val is not None and t.value != val):
            raise SyntaxError(f"Expected {type} {val}, got {t}")
        return t

    def recover_error(self, start_pos):
        start_tok = self.tokens[start_pos]
        start_char = start_tok.start
        curr_pos = start_pos
        end_char = start_tok.end
        
        while curr_pos < len(self.tokens):
            t = self.tokens[curr_pos]
            if t.type == 'EOF':
                end_char = t.end
                break
                
            if t.value == ';':
                end_char = t.end
                curr_pos += 1
                break
                
            if t.value == '}':
                if curr_pos == start_pos:
                    end_char = t.end
                    curr_pos += 1
                else:
                    end_char = t.start
                break
                
            if curr_pos > start_pos:
                prev_t = self.tokens[curr_pos - 1]
                gap = self.source[prev_t.end : t.start]
                if '\n' in gap:
                    end_char = prev_t.end
                    break
            
            end_char = t.end
            curr_pos += 1
            
        while end_char < len(self.source) and self.source[end_char] in ' \t':
            end_char += 1
        if end_char < len(self.source) and self.source[end_char] == '\n':
            end_char += 1

        err_text = self.source[start_char:end_char]
        self.pos = min(curr_pos, len(self.tokens) - 1)
        return ErrorNode(err_text)

    def parse(self):
        nodes = []
        while self.peek().type != 'EOF':
            if self.peek().value == '}':
                nodes.append(self.recover_error(self.pos))
                continue
                
            start_pos = self.pos
            try:
                nodes.append(self.statement())
            except SyntaxError:
                nodes.append(self.recover_error(start_pos))
        return nodes

    def block(self):
        self.expect('OP', '{')
        stmts = []
        while self.peek().value != '}':
            if self.peek().type == 'EOF':
                raise SyntaxError("Unexpected EOF in block")
            start_pos = self.pos
            try:
                stmts.append(self.statement())
            except SyntaxError:
                stmts.append(self.recover_error(start_pos))
        self.expect('OP', '}')
        return Block(stmts)

    def statement(self):
        if self.peek().type == 'ID' and self.peek().value == 'print':
            self.next()
            node = Print(self.expression())
            self.expect('OP', ';')
            return node
        elif self.peek().type == 'ID' and self.peek().value == 'return':
            self.next()
            node = Return(self.expression())
            self.expect('OP', ';')
            return node
        elif self.peek().type == 'ID' and self.peek().value == 'while':
            self.next()
            self.expect('OP', '(')
            cond = self.expression()
            self.expect('OP', ')')
            body = self.block()
            return While(cond, body)
        elif self.peek().type == 'ID' and self.peek().value == 'if':
            self.next()
            self.expect('OP', '(')
            cond = self.expression()
            self.expect('OP', ')')
            body = self.block()
            return If(cond, body)
        elif self.peek().type == 'ID' and self.peek().value == 'for':
            self.next()
            self.expect('OP', '(')
            
            init_targets = []
            while True:
                init_targets.append(self.factor())
                if self.peek().value == ',':
                    self.next()
                else:
                    break
            self.expect('OP', '=')
            init_exprs = [self.expression()]
            while self.peek().value == ',':
                self.next()
                init_exprs.append(self.expression())
            
            init = Assign(init_targets, init_exprs)
            self.expect('OP', ';')
            
            cond = self.expression()
            self.expect('OP', ';')
            
            step_targets = []
            while True:
                step_targets.append(self.factor())
                if self.peek().value == ',':
                    self.next()
                else:
                    break
            self.expect('OP', '=')
            step_exprs = [self.expression()]
            while self.peek().value == ',':
                self.next()
                step_exprs.append(self.expression())
            
            step = Assign(step_targets, step_exprs)
            self.expect('OP', ')')
            
            body = self.block()
            return For(init, cond, step, body)
        elif self.peek().type == 'ID' and self.peek().value == 'interp':
            self.next()
            name = self.expect('ID').value
            self.expect('OP', '(')
            params = []
            if self.peek().value != ')':
                params.append(self.expect('ID').value)
                while self.peek().value == ',':
                    self.next()
                    params.append(self.expect('ID').value)
            self.expect('OP', ')')
            self.expect('OP', '=')
            
            if self.peek().value == '{':
                body = self.block()
            else:
                body = self.expression()
                self.expect('OP', ';')
            return InterpDef(name, params, body)
        elif self.peek().type in ('ID', 'OP'):
            targets = []
            while True:
                targets.append(self.factor())
                if self.peek().value == ',':
                    self.next()
                else:
                    break
            self.expect('OP', '=')
            
            exprs = [self.expression()]
            while self.peek().value == ',':
                self.next()
                exprs.append(self.expression())
                
            node = Assign(targets, exprs)
            self.expect('OP', ';')
            return node
        else:
            raise SyntaxError(f"Invalid statement: {self.peek()}")

    def expression(self):
        left = self.term()
        while self.peek().type == 'OP' and self.peek().value in ['+', '-', '>', '<']:
            op = self.next().value
            right = self.term()
            left = BinOp(left, op, right)
        return left

    def term(self):
        left = self.factor()
        while self.peek().type == 'OP' and self.peek().value in ['*', '/']:
            op = self.next().value
            right = self.factor()
            left = BinOp(left, op, right)
        return left

    def factor(self):
        t = self.peek()
        if t.type == 'NUMBER':
            self.next()
            return Literal(int(t.value).to_bytes(4, 'little'))
        elif t.type == 'HEX':
            self.next()
            return Literal(int(t.value, 16).to_bytes(4, 'little'))
        elif t.type == 'STRING':
            self.next()
            return Literal(t.value[1:-1].encode('utf-8'))
        elif t.type == 'ID':
            self.next()
            node = Var(t.value)
            if self.peek().value == '[':
                self.next()
                idx_expr = self.expression()
                self.expect('OP', ']')
                return Index(node, idx_expr)
            return node
        elif t.type == 'OP' and t.value == '(':
            self.next()
            if self.peek().type == 'ID' and self.tokens[self.pos+1].value == ')':
                if self.peek().value in ['int', 'raw', 'heap', 'createheap', 'float', 'bool', 'char', 'string']:
                    interp = self.next().value
                    self.expect('OP', ')')
                    return InterpCall(interp, [self.factor()])
                else:
                    interp = self.next().value
                    self.expect('OP', ')')
                    args = []
                    if self.peek().value == '(':
                        self.next()
                        if self.peek().value != ')':
                            args.append(self.expression())
                            while self.peek().value == ',':
                                self.next()
                                args.append(self.expression())
                        self.expect('OP', ')')
                    else:
                        args.append(self.factor())
                    return InterpCall(interp, args)
            else:
                expr = self.expression()
                self.expect('OP', ')')
                return expr
        else:
            raise SyntaxError(f"Unexpected token {t}")

# ==================== 运行时环境 (Runtime) ====================
class ReturnException(Exception):
    def __init__(self, value):
        self.value = value

class Heap:
    def __init__(self, handle, size):
        self.handle = handle
        self.size = size
        self.data = bytearray(b'\x00' * size)
        self.initial_bindings = set() 

class Env:
    def __init__(self, parent=None):
        self.vars = {}
        self.heaps = {} if parent is None else parent.heaps
        self.var_heaps = {} if parent is None else parent.var_heaps
        self.interpreters = {} if parent is None else parent.interpreters
        self.parent = parent

    def read_var(self, name, keep_heap_alive=False):
        if name in self.vars:
            val = self.vars[name]
            del self.vars[name] 
            debug(f"变量 '{name}' 被读取并销毁。")
            
            if name in self.var_heaps:
                handle = self.var_heaps[name]
                del self.var_heaps[name]
                if handle in self.heaps:
                    h = self.heaps[handle]
                    if name in h.initial_bindings:
                        h.initial_bindings.remove(name)
                        debug(f"变量 '{name}' 从堆 {handle} 的初始绑定中移除。剩余: {h.initial_bindings}")
                        if not h.initial_bindings and not keep_heap_alive:
                            del self.heaps[handle]
                            debug(f"堆 {handle} 没有任何初始绑定了，立即销毁！")
            return val
        elif self.parent:
            return self.parent.read_var(name, keep_heap_alive)
        else:
            debug(f"变量 '{name}' 不存在，返回默认全零。")
            return b'\x00\x00\x00\x00'

    def write_var(self, name, val, handle=None):
        if name in self.var_heaps:
            old_handle = self.var_heaps[name]
            del self.var_heaps[name]
            if old_handle in self.heaps:
                h = self.heaps[old_handle]
                if name in h.initial_bindings:
                    h.initial_bindings.remove(name)
                    if not h.initial_bindings:
                        del self.heaps[old_handle]

        self.vars[name] = val
        if handle is not None:
            self.var_heaps[name] = handle
            if handle in self.heaps:
                self.heaps[handle].initial_bindings.add(name)
                debug(f"变量 '{name}' 创建，绑定了堆 {handle}。当前堆绑定: {self.heaps[handle].initial_bindings}")
        else:
            debug(f"变量 '{name}' 创建/覆盖为普通二进制值。")

    def eval(self, node):
        if isinstance(node, Literal):
            return node.val
        elif isinstance(node, Var):
            return self.read_var(node.name)
        elif isinstance(node, ErrorNode):
            if not node.text.endswith('\n'):
                print(node.text + '\n', end='')
            else:
                print(node.text, end='')
        elif isinstance(node, Index):
            if isinstance(node.base, Var):
                name = node.base.name
                idx_val = self.eval(node.idx)
                idx = int.from_bytes(idx_val[:4], 'little') if len(idx_val)>=4 else int.from_bytes(idx_val+b'\x00'*(4-len(idx_val)), 'little')
                
                handle_bytes = self.read_var(name)
                handle = int.from_bytes(handle_bytes[:4], 'little')
                if handle in self.heaps:
                    h = self.heaps[handle]
                    if idx < len(h.data):
                        return bytes(h.data[idx:idx+4].ljust(4, b'\x00'))
                    return b'\x00\x00\x00\x00'
                return b'\x00\x00\x00\x00'
        elif isinstance(node, BinOp):
            left_val = self.eval(node.left)
            right_val = self.eval(node.right)
            l_int = int.from_bytes(left_val[:4], 'little') if len(left_val)>=4 else int.from_bytes(left_val+b'\x00'*(4-len(left_val)), 'little')
            r_int = int.from_bytes(right_val[:4], 'little') if len(right_val)>=4 else int.from_bytes(right_val+b'\x00'*(4-len(right_val)), 'little')
            if node.op == '+': res = l_int + r_int
            elif node.op == '-': res = l_int - r_int
            elif node.op == '*': res = l_int * r_int
            elif node.op == '/': res = l_int // r_int if r_int != 0 else 0
            elif node.op == '>': res = 1 if l_int > r_int else 0
            elif node.op == '<': res = 1 if l_int < r_int else 0
            return res.to_bytes(4, 'little')
        
        elif isinstance(node, Block):
            for stmt in node.stmts:
                self.eval(stmt)
                
        elif isinstance(node, Return):
            val = self.eval(node.expr)
            raise ReturnException(val)

        elif isinstance(node, While):
            while int.from_bytes(self.eval(node.cond)[:4], 'little') != 0:
                self.eval(node.body)

        elif isinstance(node, For):
            while True:
                self.eval(node.init)
                if int.from_bytes(self.eval(node.cond)[:4], 'little') == 0:
                    break
                self.eval(node.body)
                self.eval(node.step)
                
        elif isinstance(node, If):
            if int.from_bytes(self.eval(node.cond)[:4], 'little') != 0:
                self.eval(node.body)

        elif isinstance(node, InterpDef):
            self.interpreters[node.name] = node
            debug(f"解释器 '{node.name}' 定义成功，参数: {node.params}")
            return b'\x00'

        elif isinstance(node, InterpCall):
            interp_name = node.interp
            if interp_name == 'heap' and len(node.args) == 1 and isinstance(node.args[0], Var):
                name = node.args[0].name
                handle_bytes = self.read_var(name, keep_heap_alive=True) 
                handle = int.from_bytes(handle_bytes[:4], 'little')
                if handle in self.heaps:
                    h = self.heaps[handle]
                    data = bytes(h.data)
                    if not h.initial_bindings:
                        del self.heaps[handle]
                        debug(f"堆 {handle} 的最后一次数据被读取，堆现在释放！")
                    return data
                return b'\x00' * 100
            
            elif interp_name in ['int', 'raw', 'float', 'bool', 'char', 'string']:
                inner_val = self.eval(node.args[0])
                if interp_name == 'int':
                    num = int.from_bytes(inner_val[:4], 'little') if len(inner_val)>=4 else int.from_bytes(inner_val+b'\x00'*(4-len(inner_val)), 'little')
                    return str(num).encode('utf-8')
                elif interp_name == 'raw':
                    return inner_val.hex().encode('utf-8')
                elif interp_name == 'float':
                    pad = inner_val[:4].ljust(4, b'\x00')
                    return str(struct.unpack('<f', pad)[0]).encode('utf-8')
                elif interp_name == 'bool':
                    is_true = any(b != 0 for b in inner_val)
                    return b'true' if is_true else b'false'
                elif interp_name == 'char':
                    return inner_val[:1]
                elif interp_name == 'string':
                    return inner_val.split(b'\x00')[0]
                    
            elif interp_name in self.interpreters:
                func_def = self.interpreters[interp_name]
                if len(node.args) != len(func_def.params):
                    debug(f"错误：参数数量不匹配！")
                    return b'\x00' * 4
                
                debug(f"--> 进入解释器 '{interp_name}'")
                arg_vals = []
                for arg in node.args:
                    arg_vals.append(self.eval(arg))
                
                local_env = Env(parent=self)
                for param, val in zip(func_def.params, arg_vals):
                    local_env.write_var(param, val)
                
                result = b'\x00' * 4
                try:
                    if isinstance(func_def.body, Block):
                        local_env.eval(func_def.body)
                    else:
                        result = local_env.eval(func_def.body)
                except ReturnException as e:
                    result = e.value
                finally:
                    debug(f"<-- 退出解释器 '{interp_name}'，局部变量全部销毁")
                return result

        elif isinstance(node, Assign):
            if len(node.exprs) == 1 and isinstance(node.exprs[0], Var) and node.exprs[0].name.startswith('_createheap_'):
                parts = [p for p in node.exprs[0].name.split('_') if p]
                handle = int(parts[1])
                size = int(parts[2])
                h = Heap(handle, size)
                self.heaps[handle] = h
                debug(f"成功创建堆 {handle}，大小 {size} 字节。")
                
                val = handle.to_bytes(4, 'little')
                for target in node.targets:
                    if isinstance(target, Var):
                        self.write_var(target.name, val, handle=handle)
            else:
                vals = []
                if len(node.exprs) == 1:
                    val = self.eval(node.exprs[0])
                    vals = [val] * len(node.targets)
                else:
                    for expr in node.exprs:
                        vals.append(self.eval(expr))
                
                for i, target in enumerate(node.targets):
                    val = vals[i]
                    if isinstance(target, Var):
                        self.write_var(target.name, val)
                    elif isinstance(target, Index):
                        if isinstance(target.base, Var):
                            name = target.base.name
                            idx_val = self.eval(target.idx)
                            idx = int.from_bytes(idx_val[:4], 'little') if len(idx_val)>=4 else int.from_bytes(idx_val+b'\x00'*(4-len(idx_val)), 'little')
                            
                            handle_bytes = self.read_var(name)
                            handle = int.from_bytes(handle_bytes[:4], 'little')
                            if handle in self.heaps:
                                h = self.heaps[handle]
                                if idx + 4 > len(h.data):
                                    new_size = max(len(h.data) * 2, idx + 4)
                                    new_heap = Heap(handle, new_size)
                                    new_heap.data[:len(h.data)] = h.data
                                    new_heap.initial_bindings = set(h.initial_bindings)
                                    self.heaps[handle] = new_heap
                                    h = new_heap
                                    debug(f"数组越界：销毁旧堆 {handle} 并创建新堆（大小 {new_size}），继承了初始绑定: {h.initial_bindings}")
                                
                                val_padded = val[:4].ljust(4, b'\x00')
                                h.data[idx:idx+4] = val_padded
                                debug(f"成功写入数组 {name}[{idx}] = {int.from_bytes(val_padded, 'little')}")
                    elif isinstance(target, InterpCall) and target.interp == 'heap':
                        if isinstance(target.expr, Var):
                            name = target.expr.name
                            handle_bytes = self.read_var(name) 
                            handle = int.from_bytes(handle_bytes[:4], 'little')
                            if handle in self.heaps:
                                h = self.heaps[handle]
                                h.data = bytearray(val.ljust(h.size, b'\x00')[:h.size])
                                debug(f"成功将数据写入堆 {handle}")
                                
        elif isinstance(node, Print):
            val = self.eval(node.expr)
            try:
                print(val.decode('utf-8'))
            except:
                print(val.hex())

# ==================== 启动入口 ====================
def run_file(path):
    source = path.read_text(encoding='utf-8')
    source = re.sub(r'\(createheap\)\((\d+),\s*(\d+)\)', r'_createheap_\1_\2', source)
    
    tokens = lex(source)
    parser = Parser(tokens, source)
    ast = parser.parse()
    env = Env()
    
    for node in ast:
        try:
            env.eval(node)
        except ReturnException:
            debug("错误：return 出现在了函数外部！")
            break
    return 0

def main():
    global DEBUG
    if '--debug' in sys.argv:
        DEBUG = True
        sys.argv.remove('--debug') # 移除参数，防止影响后面的长度检查

    if len(sys.argv) < 3 or sys.argv[1] != 'run':
        print("用法: python oncelang.py run <file.once> [--debug]")
        return 0
    path = Path(sys.argv[2])
    if not path.exists():
        print("文件不存在")
        return 0
    return run_file(path)

if __name__ == "__main__":
    sys.exit(main())