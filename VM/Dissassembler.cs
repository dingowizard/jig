using System.Text;
using Sys = System.Collections.Generic;
namespace VM;

public static class Dissassembler {

    public static string[] Disassemble(Template template) {
        Sys.List<string> literals = ["***LITERALS***"];
        for (int i = 0; i < template.Slots.Length; i++) {
            literals.Add($"{i:D5}\t{template.Slots[i].Print()}");
            
        }
        literals.Add("--------------------------");
        Sys.List<string> globals = ["***BINDINGS***"];
        for (int i = 0; i < template.Bindings.Length; i++) {
            globals.Add($"{i:D5}\t{template.Bindings[i].Symbol.Print()}");
        }
        globals.Add("--------------------------");
        Sys.List<string> instructions = ["***INSTRUCTIONS***"];
        for (int n = 0; n < template.Code.Length; n++) {
            instructions.Add(Decode(n, template.Code[n]));
        }
        instructions.Add("--------------------------");
        return literals.Concat(globals).Concat(instructions).ToArray();

    }
    
    public static string Decode(int lineNo, ulong instr) {
        OpCode opCode = (OpCode)(instr >> 56);
        switch (opCode) {
            case OpCode.Push:
                return $"{lineNo:D5}\tPUSH";
            case OpCode.Pop:
                return $"{lineNo:D5}\tPOP";
            case OpCode.Call:
                return $"{lineNo:D5}\tCALL";
            case OpCode.Lit:
                return $"{lineNo:D5}\tLIT\t{instr & 0x00FFFFFFFFFFFFFF:D3}";
            case OpCode.PushContinuation:
                return $"{lineNo:D5}\tSAVE\t{instr & 0x00FFFFFFFFFFFFFF:D3}";
            case OpCode.PopContinuation:
                return $"{lineNo:D5}\tRET";
            case OpCode.Bind:
                return $"{lineNo:D5}\tBIND\t{instr & 0x00FFFFFFFFFFFFFF:D3}";
            case OpCode.BindRest:
                return $"{lineNo:D5}\tBNDR\t{instr & 0x00FFFFFFFFFFFFFF:D3}";
            case OpCode.Env:
                return $"{lineNo:D5}\tENVT";
            case OpCode.Lambda:
                return $"{lineNo:D5}\tLMBD";
            case OpCode.Load:
                return $"{lineNo:D5}\tLOAD\t{instr & 0x00FFFFFFFFFFFFFF:D3}";
            case OpCode.Store:
                return $"{lineNo:D5}\tSTORE\t{instr & 0x00FFFFFFFFFFFFFF:D3}";
            case OpCode.Jump:
                return $"{lineNo:D5}\tJMP\t{instr & 0x00FFFFFFFFFFFFFF:D3}";
            case OpCode.JumpIfFalse:
                return $"{lineNo:D5}\tJIFF\t{instr & 0x00FFFFFFFFFFFFFF:D3}";
            default:
                return $"unhandled opcode: {opCode}";
        }
    }

}