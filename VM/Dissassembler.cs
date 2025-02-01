using System.Text;
using Sys = System.Collections.Generic;
namespace VM;

public static class Dissassembler {

    public static string[] Disassemble(Template template) {
        Sys.List<string> literals = ["***SLOTS***"];
        for (int i = 0; i < template.Slots.Length; i++) {
            literals.Add($"{i:D5}\t{template.Slots[i].Print()}");
            
        }
        literals.Add("--------------------------");
        Sys.List<string> globals = ["***GLOBALS***"];
        for (int i = 0; i < template.Globals.Length; i++) {
            literals.Add($"{i:D5}\t{template.Globals[i].Symbol.Print()}");
        }
        globals.Add("--------------------------");
        Sys.List<string> instructions = ["***INSTRUCTIONS***"];
        for (int n = 0; n < template.Code.Length; n++) {
            instructions.Add(Decode(n, template.Code[n]));
        }
        instructions.Add("--------------------------");
        return literals.Concat(globals).Concat(instructions).ToArray();

    }
    
    // Push,
    // Pop,
    // Lit,
    // PushContinuation,
    // PopContinuation,
    // Call,
    // Top,
    // LexVar,
    // Bind,
    // BindRest,
    // DefLocal,
    // DefTop,
    // SetTop,
    // SetLex,
    // Jump,
    // JumpIfFalse,
    // Env,
    // Closure,
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
                return $"{lineNo:D5}\tPUSHK\t{instr & 0x00FFFFFFFFFFFFFF:D3}";
            case OpCode.PopContinuation:
                return $"{lineNo:D5}\tRET";
            case OpCode.LexVar:
                return $"{lineNo:D5}\tVAR\t{instr & 0x00FFFFFFFFFFFFFF:D3}";
            case OpCode.Bind:
                return $"{lineNo:D5}\tBIND\t{instr & 0x00FFFFFFFFFFFFFF:D3}";
            case OpCode.Env:
                return $"{lineNo:D5}\tENVT";
            case OpCode.Closure:
                return $"{lineNo:D5}\tCLOS";
            case OpCode.Top:
                return $"{lineNo:D5}\tTOP\t{instr & 0x00FFFFFFFFFFFFFF:D3}";
            case OpCode.Jump:
                return $"{lineNo:D5}\tJMP\t{instr & 0x00FFFFFFFFFFFFFF:D3}";
            case OpCode.JumpIfFalse:
                return $"{lineNo:D5}\tJFF\t{instr & 0x00FFFFFFFFFFFFFF:D3}";
            default:
                return $"unhandled opcode: {opCode}";
        }
    }

}