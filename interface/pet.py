from ctypes import *
import isl
import platform


def get_lib_ext():
    if platform.system() == 'Windows':
        lib_ext = '.dll'
    elif platform.system() == 'Linux':
        lib_ext = '.so'
    elif platform.system() == 'Darwin':
        lib_ext = '.dylib'
    else:
        lib_ext = ''
    return lib_ext


pet = cdll.LoadLibrary("libpet" + get_lib_ext())


class overflow:
    avoid = 0
    ignore = 1


class options:
    @staticmethod
    def set_autodetect(val):
        ctx = isl.Context.getDefaultInstance()
        pet.pet_options_set_autodetect(ctx, val)

    @staticmethod
    def set_encapsulate_dynamic_control(val):
        ctx = isl.Context.getDefaultInstance()
        pet.pet_options_set_encapsulate_dynamic_control(ctx, val)

    @staticmethod
    def set_signed_overflow(val):
        ctx = isl.Context.getDefaultInstance()
        pet.pet_options_set_signed_overflow(ctx, val)


class stmt:
    def __init__(self, *args, **keywords):
        if "ptr" in keywords:
            self.ptr = keywords["ptr"]
            return

    def __repr__(self) -> str:
        return f"stmt(@{self.ptr})"

    def get_space(self) -> isl.space:
        return isl.space(pet.pet_stmt_get_space(self.ptr))

    def get_domain(self) -> isl.set:
        ptr = pet.pet_stmt_get_domain(self.ptr)
        return isl.set(ptr=isl.isl.isl_set_copy(ptr), ctx=isl.Context.getDefaultInstance())

    def print_body(self, p: isl.printer, ref2expr: isl.id_to_ast_expr) -> isl.printer:
        ptr = pet.pet_stmt_print_body(self.ptr, p.ptr, ref2expr.ptr)
        return isl.printer(ptr=isl.isl.isl_printer_copy(ptr), ctx=p.ctx)


pet.pet_stmt_get_space.restype = c_void_p
pet.pet_stmt_get_space.argtypes = [c_void_p]
pet.pet_stmt_print_body.restype = c_void_p
pet.pet_stmt_print_body.argtypes = [c_void_p, c_void_p, c_void_p]
pet.pet_stmt_get_domain.restype = c_void_p
pet.pet_stmt_get_domain.argtypes = [c_void_p]


class scop:
    def __init__(self, *args, **keywords):
        if "ptr" in keywords:
            self.ctx = keywords["ctx"]
            self.ptr = keywords["ptr"]
            self.filename = keywords["filename"]
            self.function = keywords["function"]
            return

    def __del__(self):
        pet.pet_scop_free(self.ptr)

    def __repr__(self):
        return ('pet.scop.extract_from_C_source("%s", "%s")'
                % (self.filename, self.function))

    @staticmethod
    def extract_from_C_source(filename, function):
        ctx = isl.Context.getDefaultInstance()
        filename_s = filename.encode('ascii')
        function_s = function.encode('ascii')
        res = pet.pet_scop_extract_from_C_source(ctx, filename_s, function_s)
        return scop(ctx=ctx, ptr=res, filename=filename, function=function)

    def get_instance_set(self):
        return isl.union_set(ctx=self.ctx,
                             ptr=pet.pet_scop_get_instance_set(self.ptr))

    def get_may_reads(self):
        return isl.union_map(ctx=self.ctx,
                             ptr=pet.pet_scop_get_may_reads(self.ptr))

    def get_may_writes(self):
        return isl.union_map(ctx=self.ctx,
                             ptr=pet.pet_scop_get_may_writes(self.ptr))

    def get_must_writes(self):
        return isl.union_map(ctx=self.ctx,
                             ptr=pet.pet_scop_get_must_writes(self.ptr))

    def get_must_kills(self):
        return isl.union_map(ctx=self.ctx,
                             ptr=pet.pet_scop_get_must_kills(self.ptr))

    def get_tagged_may_reads(self):
        return isl.union_map(ctx=self.ctx,
                             ptr=pet.pet_scop_get_tagged_may_reads(self.ptr))

    def get_tagged_may_writes(self):
        return isl.union_map(ctx=self.ctx,
                             ptr=pet.pet_scop_get_tagged_may_writes(self.ptr))

    def get_tagged_must_writes(self):
        return isl.union_map(ctx=self.ctx,
                             ptr=pet.pet_scop_get_tagged_must_writes(self.ptr))

    def get_tagged_must_kills(self):
        return isl.union_map(ctx=self.ctx,
                             ptr=pet.pet_scop_get_tagged_must_kills(self.ptr))

    def get_context(self):
        return isl.set(ctx=self.ctx, ptr=pet.pet_scop_get_context(self.ptr))

    def get_schedule(self):
        return isl.schedule(ctx=self.ctx,
                            ptr=pet.pet_scop_get_schedule(self.ptr))

    def get_n_stmt(self) -> int:
        return pet.pet_scop_get_n_stmt(self.ptr)

    def get_stmt(self, pos: int) -> stmt:
        return stmt(ptr=pet.pet_scop_get_stmt(self.ptr, pos))


pet.pet_options_set_autodetect.argtypes = [isl.Context, c_int]
pet.pet_options_set_encapsulate_dynamic_control.argtypes = [isl.Context, c_int]
pet.pet_options_set_signed_overflow.argtypes = [isl.Context, c_int]
pet.pet_scop_extract_from_C_source.restype = c_void_p
pet.pet_scop_extract_from_C_source.argtypes = [isl.Context, c_char_p, c_char_p]
pet.pet_scop_get_instance_set.restype = c_void_p
pet.pet_scop_get_instance_set.argtypes = [c_void_p]
pet.pet_scop_get_may_reads.restype = c_void_p
pet.pet_scop_get_may_reads.argtypes = [c_void_p]
pet.pet_scop_get_may_writes.restype = c_void_p
pet.pet_scop_get_may_writes.argtypes = [c_void_p]
pet.pet_scop_get_must_writes.restype = c_void_p
pet.pet_scop_get_must_writes.argtypes = [c_void_p]
pet.pet_scop_get_must_kills.restype = c_void_p
pet.pet_scop_get_must_kills.argtypes = [c_void_p]
pet.pet_scop_get_tagged_may_reads.restype = c_void_p
pet.pet_scop_get_tagged_may_reads.argtypes = [c_void_p]
pet.pet_scop_get_tagged_may_writes.restype = c_void_p
pet.pet_scop_get_tagged_may_writes.argtypes = [c_void_p]
pet.pet_scop_get_tagged_must_writes.restype = c_void_p
pet.pet_scop_get_tagged_must_writes.argtypes = [c_void_p]
pet.pet_scop_get_tagged_must_kills.restype = c_void_p
pet.pet_scop_get_tagged_must_kills.argtypes = [c_void_p]
pet.pet_scop_get_context.restype = c_void_p
pet.pet_scop_get_context.argtypes = [c_void_p]
pet.pet_scop_get_schedule.restype = c_void_p
pet.pet_scop_get_schedule.argtypes = [c_void_p]
pet.pet_scop_free.argtypes = [c_void_p]
pet.pet_scop_get_n_stmt.restype = int
pet.pet_scop_get_n_stmt.argtypes = [c_void_p]
pet.pet_scop_get_stmt.restype = c_void_p
pet.pet_scop_get_stmt.argtypes = [c_void_p, c_int]
