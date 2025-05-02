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
            globals.Add($"{i:D5}\t{template.Bindings[i].Symbol.Print()} {(template.Bindings[i].Top ? "(top)" : "")}");
        }
        globals.Add("--------------------------");
        Sys.List<string> instructions = [
            "***INSTRUCTIONS***",
            "ADDR\tOPCODE\tARG",
            "----\t------\t---"
        ];
        for (int n = 0; n < template.Code.Length; n++) {
            instructions.Add(Decode(n, template.Code[n], template.Slots, template.Bindings));
        }
        instructions.Add("--------------------------");
        return literals.Concat(globals).Concat(instructions).ToArray();

    }
    
    public static string Decode(int lineNo, ulong instr, Jig.Form[] literals, Binding[] bindings) {
        OpCode opCode = (OpCode)(instr >> 56);
        ulong operand = instr & 0x00FFFFFFFFFFFFFF;
        int index = (int)(instr & 0x00000000FFFFFFFF);
        int depth = (int)(instr >> 32) & 0x00FFFFFF ;
        switch (opCode) {
            case OpCode.Push:
                return $"{lineNo:D3}\tPUSH";
            case OpCode.Pop:
                return $"{lineNo:D3}\tPOP";
            case OpCode.SPToFP:
                return $"{lineNo:D3}\tSP2FP";
            case OpCode.PopFP:
                return $"{lineNo:D3}\tPOPFP";
            case OpCode.PushFP:
                return $"{lineNo:D3}\tPUSHFP";
            case OpCode.Call:
                return $"{lineNo:D3}\tCALL";
            case OpCode.Lit:
                return $"{lineNo:D3}\tLIT\t{operand:D3} ; {literals[operand].Print()}";
            case OpCode.PushContinuationForArg:
                return $"{lineNo:D3}\tCONTA\t{operand:D3}";
            case OpCode.PushContinuationForNonTailBody:
                return $"{lineNo:D3}\tCONTNT\t{operand:D3}";
            case OpCode.PushContinuationForBodyThunk:
                return $"{lineNo:D3}\tCONTBT\t{operand:D3}";
            case OpCode.PopContinuation:
                return $"{lineNo:D3}\tRET";
            case OpCode.Bind:
                return $"{lineNo:D3}\tBIND\t{operand:D3}";
            case OpCode.BindRest:
                return $"{lineNo:D3}\tBNDR\t{operand:D3}";
            case OpCode.Env:
                return $"{lineNo:D3}\tENVT";
            case OpCode.Lambda:
                return $"{lineNo:D3}\tLMBD";
            case OpCode.Top:
                return $"{lineNo:D3}\tTOP\t{operand:D3} ; {bindings[operand].Symbol.Print()}";
            case OpCode.Lex:
                return $"{lineNo:D3}\tLEX\t{depth:D3}\t{index:D3}" ;
            case OpCode.SetLex:
                return $"{lineNo:D3}\tLEX!\t{depth:D3}\t{index:D3}" ;
            case OpCode.SetTop:
                return $"{lineNo:D3}\tTOP!\t{operand:D3} ; {bindings[operand].Symbol.Print()}";
            case OpCode.Jump:
                return $"{lineNo:D3}\tJMP\t{operand:D3}";
            case OpCode.JumpIfFalse:
                return $"{lineNo:D3}\tJIFF\t{operand:D3}";
            case OpCode.PopWinder:
                return $"{lineNo:D3}\tPOPW";
            case OpCode.PushWinder:
                return $"{lineNo:D3}\tPUSHW";
            default:
                return $"unhandled opcode: {opCode}";
        }
    }

}