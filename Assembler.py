destMap = {'null':'000','M': '001', 'D':'010', 'MD': '011', 'A':'100', 'AM':'101', 'AD':'110', 'AMD':'111' }
jumpMap = {'null':'000', 'JGT': '001', 'JEQ':'010', 'JGE':'011', 'JLT':'100', 'JNE':'101', 'JLE':'110', 'JMP':'111'}
compMap = {'0':'0101010', '1':'0111111', '-1':'0111010', 'D':'0001100', 'A':'0110000', '!D':'0001101',
           '!A':'0110001', '-D':'0001111', '-A':'0110011', 'D+1':'0011111', 'A+1':'0110111', 'D-1':'0001110',
           'A-1':'0110010', 'D+A':'0000010', 'D-A':'0010011', 'A-D':'0000111', 'D&A':'0000000', 'D|A':'0010101',
           'M':'1110000', '!M':'1110001', '-M':'1110011', 'M+1':'1110111', 'M-1':'1110010', 'D+M':'1000010', 'D-M':'1010011',
           'M-D':'1000111', 'D&M':'1000000', 'D|M':'1010101'}
symbolTable = {'R0':0, 'R1':1, 'R2':2, 'R3':3, 'R4':4, 'R5':5, 'R6':6, 'R7':7, 'R8':8,
              'R9':9, 'R10':10, 'R11':11, 'R12':12, 'R13':13, 'R14':14, 'R15':15,
              'SP':0, 'LCL': 1, 'ARG':2, 'THIS':3, 'THAT':4, 'SCREEN':16384, 'KBD':24576}
def assemble(filename):
    file_in = open(filename, 'r')
    linesymbol = 0;
    for line in file_in:
        no_comment = line.split('//')[0]
        instr = no_comment.strip()
        if len(instr) > 0:
            if instr.startswith("("):
                sym = instr[1:len(instr)-1]
                symbolTable[sym] = linesymbol
                linesymbol -= 1
            linesymbol += 1
    file_in.close()

    file_in = open(filename, 'r')
    assembledData = []
    variablecount = 16
    for line in file_in:
        no_comment = line.split('//')[0]
        instr = no_comment.strip()
        tosave = ""
        if len(instr) > 0 and not instr.startswith("("):
            if instr.startswith("@"):
                if not instr[1:].isdigit():
                    if instr[1:] in symbolTable:
                        val = symbolTable[instr[1:]]
                        tosave = get16bitBinaryString(val)
                    else:
                        symbolTable[instr[1:]] = variablecount
                        tosave = get16bitBinaryString(variablecount)
                        variablecount += 1
                else:
                    tosave = get16bitBinaryString(instr[1:])
            if "=" in instr:
                tosave += "111"
                comp = instr.split("=")[1]
                dest = instr.split("=")[0]
                tosave += compMap[comp]+destMap[dest]+jumpMap["null"]
            if ";" in instr:
                tosave += "111"
                comp = instr.split(";")[0]
                jump = instr.split(";")[1]
                tosave += compMap[comp]+destMap["null"]+jumpMap[jump]
        if(len(tosave.strip()) > 0):
            assembledData.append(tosave)
    wrtitetoFile(assembledData, file_in, filename)

def wrtitetoFile(assembledData, file_in, filename):
    file_stem = filename.split('.')[0]
    output_file = file_stem + '.hack'
    file_out = open(output_file, 'w')
    for data in assembledData:
        file_out.write(data + '\n')
    file_in.close()
    file_out.close()

def get16bitBinaryString(number):
    var = bin(int(number))
    binary = var.split("b")[1]
    tosave = ""
    for i in range(16):
        if len(binary) + i == 16:
            tosave += binary
            break
        else:
            tosave += "0"
    return tosave

if __name__ == '__main__':
    filename = input("Enter filename:")
    assemble(filename)