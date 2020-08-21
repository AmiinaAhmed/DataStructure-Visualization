INCLUDE Irvine32.inc
BUFSIZE = 10000
	.data
;#####################################AMINA's DATA##################################
FileSize DWORD 0
OutputFileName byte 1000 dup(?)
Input_Char DWORD 20 Dup(?) ,0
Counter_Length dword 0
Counter dword 0
UpdatedMsg           BYTE 10000 DUP (?)
Ignor_J              BYTE  (0)
Ignor_I              BYTE  (0)
Encrypted_Message    BYTE 10000 DUP (?)
Decrypted_Message    BYTE 10000 DUP (?)
First_Letter         DWORD (0)
Secound_Letter       DWORD (0)
First_ROW_NUMBER     DWORD   ? 
Secound_ROW_NUMBER   DWORD   ?
First_Bigger         DWORD   ?
Secound_Bigger       DWORD   ? 
First_V              BYTE    ?
Encrypt_Decrypt_Check BYTE 0
LEN DWORD 0
MessageAferDec byte 10000 dup(?)

;####################################Habiba's Data###############################
Message byte 10000 dup(0)
message_AllowedLength dword 10000
message_Length dword 0
preparedString byte 10000 dup(0)
preparedString_length dword 0
count_arr_length dword 0
;##################################DINA's DATA###################################
Key byte 1000 Dup(0)
matrix_key byte 25 DUP (?)
char_alp byte  'A' , 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'
                 byte  '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0','0'
col_num dword 0
row_num dword 0
LEN_Key DWORD 0
find byte 0
ch1 byte 0

;data for using files
	fileName BYTE 1000 DUP (?)
	FileHandle HANDLE ?				
	En byte '-Encrypted.txt'
	De byte '-Decrypted.txt'
;######################################################################################
Virtual_Message byte ?
Virtual_Message_AllowedSize Dword 10000
ket byte 1000 Dup(0)
M1 BYTE "D:\playfair_en.txt"
M2 DWORD  18
M3 BYTE		 ?
;########################################THE MAIN PROC#########################################	
;EncryptBouns proto filename_U: ptr byte	,size_file :Dword , OutputC: ptr byte
DecryptBouns proto filename_U: ptr byte	,size_file :Dword , OutputC: ptr byte
Encryption proto filename_U: ptr byte	,size_file :Dword , OutputC: ptr byte
Decryption proto filename_U: ptr byte ,size_file :Dword  , OutputC: ptr byte
.code
; DLLMAIN is required for any DLL
DLLMAIN PROC hInstance : DWORD, fdwReason : DWORD, lpReserved : DWORD
mov eax, 1; Return true to caller.
exit
DLLMAIN ENDP

;-----------------------------------------------------------------------------------------------------------------------------------
;EncryptBouns: USE Vigenere  TO ENCRYPT MESSAGE
;  Receives :   ESI:PATH OF FILE  
;                      ECX:  LEN  OF FILE 
;  RETURN : PATH OF OUT FILE
;--------------------------------------------------------------------------------------------------------------------------------
EncryptBouns proc filename_U: ptr byte	,size_file :Dword 	 , OutputC : ptr byte
pushad 
pushfd
MOV LEN_KEY,0
MOV LEN,0
Mov counter,0
         mov ecx,   size_file
         mov FileSize ,ecx
         cld
         mov esi,filename_U
         mov edi, offset fileName
         rep movsb

         call ReadFromF
         MOV Esi,OFFSET Key                 
		 MOV EDI ,OFFSET KET
		 mov ECX,LEN_Key  
		 call RemoveSpaces
         mov LEN_KEY,ebx
         MOV ecx, LEN_Key 
         MOV ESI ,OFFSET Ket 
        CALL CapitalAllLetters
		 mov ecx, LEN
		 mov esi,offset Ket
         mov edi, offset Message
		 mov ebx, 0
		 mov dl,0
			L: 
				mov dx, 0
				movzx ax,byte ptr[ESI]
				movzx bx,byte ptr [edi]
				CMP bl, ' '
				JE  JUMP
				add ax, bx
				sub ax, (2 * 'A')
				mov bx, 26
				div bx
				add dx, 'A'
				mov [edi], dl
				inc esi
                inc counter
                mov ebx,counter
                CMP ebx, LEN_KEY
				JNe JUMP
				MOV ESI,offset Ket
		JUMP :
				INC edi
			loop L
       mov ecx,LEN
       cld
       mov edi, offset Encrypted_Message
       mov esi,offset  Message
       rep movsb
       CALL SaveInFileProcEn  
      	cld
       mov ecx,size_file
       add ecx,10
       mov edi,  OutputC
       mov esi,offset OutputFileName
       rep movsb
    popfd   
	popad
			ret
	EncryptBouns endp
;-----------------------------------------------------------------------------------------------------------------------------------
;DecryptBouns: USE Vigenere  TO DECRYPT MESSAGE
;  Receives :   ESI:PATH OF FILE  
;                      ECX:  LEN  OF FILE 
;  RETURN : PATH OF OUT FILE
;--------------------------------------------------------------------------------------------------------------------------------
DecryptBouns proc filename_U: ptr byte	,size_file :Dword  , OutputC: ptr byte
pushad
pushfd
MOV LEN_KEY,0
MOV LEN,0
Mov counter,0
         mov ecx,   size_file
         mov FileSize ,ecx
         cld
         mov esi,filename_U
         mov edi, offset fileName
         rep movsb

         call ReadFromF
         MOV Esi,OFFSET Key                 
		 MOV EDI ,OFFSET KET
		 mov ECX,LEN_Key  
		 call RemoveSpaces
         mov LEN_KEY,ebx
         MOV ecx, LEN_Key 
         MOV ESI ,OFFSET Ket 
        CALL CapitalAllLetters
		 mov ecx, LEN
		 mov esi,offset Message
         mov edi, offset Ket
		 mov ebx, 0
		 mov dl,0
			L: 
				mov dx, 0
				movzx ax,byte ptr[ESI]
				movzx bx,byte ptr [edi]
				CMP al, ' '
				JE  JUMP
				sub ax, bx
				add ax, 26
				mov bx, 26
				div bx
				add dx, 'A'
				mov [esi], dl
				inc edi
                inc counter
                mov ebx,counter
                CMP ebx, LEN_KEY
				JNe JUMP
				MOV EdI,offset Ket
                Mov counter,0
		JUMP :
				INC esi
			loop L
  
       mov ecx,LEN
       cld
       mov edi, offset MessageAferDec
       mov esi,offset  Message
       rep movsb
       CALL SaveInFileProcDe 
      	cld
       mov ecx,size_file
       add ecx,10
       mov edi,  OutputC
       mov esi,offset OutputFileName
       rep movsb
      
popfd
popad
			ret
	DecryptBouns endp
;-----------------------------------------------------------------------------------------------------------------------------------
;ENCRYPTION: USE Playfair Ciphering   TO ENCRYPT  MESSAGE
;  Receives :   ESI:PATH OF FILE  
;                      ECX:  LEN  OF FILE 
;  RETURN : PATH OF OUT FILE
;--------------------------------------------------------------------------------------------------------------------------------
Encryption PROC	  filename_U: ptr byte	,size_file :Dword  , OutputC: ptr byte

pushad
pushfd

mov ecx,   size_file
mov FileSize ,ecx
;------------------------------------------------
cld
mov esi,filename_U
mov edi, offset fileName
rep movsb
;---------------------------------------------------
MOV Encrypt_Decrypt_Check,1
L7:
call ReadFromF

MOV ECX,  message_AllowedLength
MOV ESI, offset Message
;----------------------------------
CALL CapitalAllLetters

MOV ECX, Message_Length
CALL PrepareMessage

MOV ESI,OFFSET preparedString                 
MOV EDI ,OFFSET UpdatedMsg
mov ECX, preparedString_length            ;The Real length 
CALL RemoveSpaces                               ;If there is spaces between the words this function will remove it
		
MOV LEN , EBX

MOV ecx, LEN_Key 
MOV ESI ,OFFSET Key 
CALL CapitalAllLetters
 mov ecx,LEN_Key
CALL Gen_Key_ToMatrix
mov eax,LEN_Key
CALL Complete_matrix

	   MOV ESI ,OFFSET UpdatedMsg
       MOV EDI ,OFFSET Encrypted_Message
	   MOV EAX,LEN
	   MOV Encrypt_Decrypt_Check ,1
	   CALL Encryption_Decryption
	   WriteF	:
	   CALL SaveInFileProcEn  
        cld
       mov ecx,size_file
       add ecx,10
       mov edi,  OutputC
       mov esi,offset OutputFileName
       rep movsb
popfd
popad     
ret
Encryption ENDP
;--------------------------------------------------------------------
; ReadFromF : 	OPEN THE FILE AND READ FROM IT
; 
;--------------------------------------------------------------------
ReadFromF PROC

call Open_File
CALL Read_From_File
JNC Check_Buffer_Size
JMP Close_File

Check_Buffer_Size:
CMP EAX, BUFSIZE
JB Buf_Size_Ok

Buf_Size_Ok:
MOV Message[eax], 0
mov message_Length,eax
MOV EDX, OFFSET Message

Close_File:
MOV EAX, FileHandle
CALL CloseFile
next:
ret
ReadFromF ENDP
;-----------------------------------------------------------------------------------------------------
;Function: Opens a text File.
;EDX = OFFSET of FileName
;EAX contains a Valid File Handle if the Operation was Successful, otherwise
;EAX contain Invalid File Handle
;-----------------------------------------------------------------------------------------------------
Open_File Proc
MOV EDX, OFFSET fileName
CALL OpenInputFile
MOV FileHandle, EAX
ret
Open_File ENDP

;-----------------------------------------------------------------------------------------------------
;Function: Read From a Text File and passes what read to Buffer Array
;EDX = OFFSET of Buffer Array
;ECX = Buffer Size
;-----------------------------------------------------------------------------------------------------
Read_From_File PROC
Mov EDX,Offset Virtual_Message
MOV ECX, Virtual_Message_AllowedSize
CALL ReadFromFile

mov ecx, eax
MOV Esi, OFFSET Virtual_Message
MOV Edi, OFFSET key
mov LEN_KEY,0
l1:
 mov bl,[Esi]
 cmp bl,10
 je l2
 mov [EDi],bl
 inc Edi
 inc LEN_KEY
 inc Esi
 loop l1
l2:
sub eax,LEN_KEY
dec eax
dec LEN_KEY
inc esi

Mov Edi, offset message
cld
rep movsb
mov Len,eax
ret
popfd
popad
Read_From_File ENDP

;--------------------------------------------------------
; Save result in file 
; Receives:nothing
; Returns: nothing 
;-------------------------------------------------------
SaveInFileProcEn Proc USES EDI ESI ECX EAX
cld
mov esi,offset fileName
mov edi,offset OutputFileName
mov ecx,FileSize
sub ecx,4
rep movsb
mov ecx,14
mov edx,offset En
L1:
mov al,[edx]
mov [edi],al
inc edx
inc edi
loop l1
mov edx,OFFSET OutputFileName ;Result.txt
call CreateOutputFile
mov fileHandle,eax
mov edx,OFFSET Encrypted_Message
mov ecx,LEN
call WriteToFile
mov eax,fileHandle
call CloseFile
ret
SaveInFileProcEn Endp

SaveInFileProcDe Proc USES EDI ESI ECX EAX
;--------------------------------------------------------
; Save result in file 
; Receives:nothing
; Returns: nothing 
;-------------------------------------------------------
cld
mov esi,offset fileName
mov edi,offset OutputFileName
mov ecx,FileSize
sub ecx,4
rep movsb
mov ecx,14
mov edx,offset De
L1:
mov al,[edx]
mov [edi],al
inc edx
inc edi
loop l1
mov edx,OFFSET OutputFileName ;Result.txt
call CreateOutputFile
mov fileHandle,eax
mov edx,OFFSET MessageAferDec
mov ecx,LEN
call WriteToFile
mov eax,fileHandle
call CloseFile
ret
SaveInFileProcDe Endp
;-----------------------------------------------------------------------------------------------------------------------------------
;Function takes string and make all letters capital 
;Return : capital all letters string
;    ESI : OFFSET SRING THAT CONVERTED TO UPPER 
;--------------------------------------------------------------------------------------------------------------------------------

CapitalAllLetters proc  USES ESI 
L1: 
mov al ,[esi] 
and al, 11011111b 
mov [esi], al 
inc esi 
Loop L1 
ret
CapitalAllLetters endp
;-----------------------------------------------------------------------------------------------------------------------------------
;Function works on massage to return it after editing
; Return : New Massage After Editing 
;    
;--------------------------------------------------------------------------------------------------------------------------------
PrepareMessage Proc
mov esi,offset message
mov edi,offset preparedString
messageLoop:
call findLetter
mov al,[esi]
mov edx,message_Length
sub edx,count_arr_length
cmp edx,0
jbe declength
inc esi
call findLetter
mov bl, [esi]
mov [edi],al
inc edi
cmp al ,bl
jnz elseFn
mov bl ,'X'
dec count_arr_length
dec esi
elseFn:
mov [edi],bl
inc edi 
mov al,' '
mov [edi],al
add preparedString_length,3
inc edi
inc esi
mov edx,message_Length
sub edx,count_arr_length
cmp edx,1
jbe complete
endLoop:
 jmp messageLoop
 jmp funEnd
 declength:
 dec count_arr_length
 complete:
 dec preparedString_length
 dec edi
 mov eax,count_arr_length
 cmp eax,message_Length
 jz funEnd
 call completeString
 funEnd:
ret
PrepareMessage endp

;###########################Find Letter######################
;-----------------------------------------------------------------------------------------------------------------------------------
; findLetter	: CHECK IF CHRACTER OF MESSAGE IS ALPHBET 
;  	   Receives :		NON       
;  		RETURN 
;--------------------------------------------------------------------------------------------------------------------------------------------

findLetter Proc
mov ecx,message_Length
sub ecx,esi
add ecx,offset message
letter:
inc count_arr_length
mov dl,[esi]
cmp dl,'A'
jb infun
cmp dl,'Z'
jbe outfn
infun:
inc esi
loop letter
outfn:
ret
findLetter endp
;############################################################

;#########################complete Length######################
;------------------------------------------------------------------------------------------------------------------
;completeString :     Edits the Massage String by adding 'X' to duplicate pair.
;   receives: edi	( preparedString)
;-------------------------------------------------------------------------------------------------------------------
completeString Proc	   uses edi
 mov bl,' '
 mov [edi],bl
 inc edi
 mov bl,[esi]
 mov [edi],bl
 inc edi 
 mov bl , 'X'
 mov [edi],bl
 add preparedString_length,3
ret
completeString endp

;-----------------------------------------------------------------------------------------------------------------------------------
;Function works on String to return it after editing by removing spaces
; Return : New Massage After Editing 
;    
;--------------------------------------------------------------------------------------------------------------------------------

RemoveSpaces PROC USES EDI ESI ECX
mov EBX,ECX
L1:

cmp byte PTR [esi] , 00100000b  ;Check the value with space
je SpaceSkip                                    ;If it's equal it will jmp to SpaceSkip
movsb
jmp next
cmp byte PTR [esi] ,00001010b

je SpaceSkip
movsb
jmp next

SpaceSkip:                            ;Skip this space and move to the next element in the word
inc esi
inc Counter_length
next:
Loop L1
Sub EBX,Counter_length
ret
RemoveSpaces ENDP

;##########################Enter_key to Matrix######################
;-----------------------------------------------------------------------------------------------------------------------------
;  Gen_Key_ToMatrix:Enter Key to matrix with unique charater
;  Receives  : ECX == LEN KEY
;  ESI    :  CINTAIN LEN OF KEY
;  EDX  :  AS INDEX IN ARRAY KEY
;  EDI   :	STORE VALUE OF COLMN
;-------------------------------------------------------------------------------------------------------------------------------
Gen_Key_ToMatrix proc	  USES	ECX

mov col_num,0
mov edx,0

matrix:
mov find,0
cmp key[edx],'J'
jne call_fun
mov key[edx],'I'
DEC LEN_Key
call_fun:
call find_specific_element
cmp find ,1
je Step1              ; ok add to matrix
DEC LEN_Key   ; else it found in matrix dec len
jmp Step2

Step1:
mov edi,col_num
MOV matrix_key[edi] , al
inc col_num

Step2:
inc edx
loop matrix
ret
Gen_Key_ToMatrix endp 

;################check char if used before or no############################
;-----------------------------------------------------------------------------------------------------------------------------------
; find_specific_element	: CHECK IF CHARACTER (edx) IF USED BERFOR OR NO
;  Receives		 ECX  : AS INDEX OF VALUE CHARATER VISTED OR NO  IN ARRAY(char_alp) 
;   RETURN ==> Find : 0 IF CHRARTER USED BEFORE	   
;                                      : 1 IF CHARACTER NOT USED BEFORE AND SET VALUE OF CHARATER IN ARRAY(char_alp)		
;  EBX	 : AS INDEX OF ALPHBET CHARATER   IN ARRAY(char_alp) 		
;--------------------------------------------------------------------------------------------------------------------------------------------						 
find_specific_element proc uses ecx   edx
mov ecx,25
mov ebx,0
alph_loop:
mov al , Key[edx]  
CMP  char_alp[ebx] ,al
je rep_lab
mov al,char_alp[ebx]
jmp ex
rep_lab:
mov edi,ebx
add edi,25
cmp char_alp[edi] , '0'      ;not visited or no taken it
je visit_character
mov find,0
JMP EX_FUN
visit_character:  
mov  char_alp[edi] , '1'
mov find , 1
JMP EX_FUN
ex:
inc ebx
loop alph_loop
EX_FUN:
ret
find_specific_element endp																												 
;######################## COMPLETE MATREX OF KEY ############################
;-----------------------------------------------------------------------------------------------------------------------------------
; Complete_matrix	: COMPLETET MATRIX WITH UNIQUE ALPHBET CHARATER
;  	   Receives 		EAX  : LEN KEY             
;   ECX  : AS INDEX OF VALUE CHARATER VISTED OR NO  IN ARRAY(char_alp) 		
;   EBX	 : AS INDEX OF ALPHBET CHARATER   IN ARRAY(char_alp) 	
;  	
;--------------------------------------------------------------------------------------------------------------------------------------------
Complete_matrix proc	  USES  EAX

mov col_num , eax
mov ecx , 25
sub ecx , LEN_Key
mov edi , 25                  ;counter : char_alp
Matrix:
inner:                             ;to get value character
cmp char_alp[edi] ,'0'
je NEXT_STEP
CMP edi,50  
je ex_lab
inc edi
jmp inner
NEXT_STEP:
mov char_alp[edi]  , '1'
sub edi,25                 ; to get character
mov esi , col_num
mov al , char_alp[edi]
mov matrix_key[esi],al
add edi , 25
ex:
inc col_num
loop matrix
ex_lab:
ret
Complete_matrix endp

;-----------------------------------------------------------------------------------------------------------------------------------
; Encryption_Decryption : If User CHOOSE Encryption operation 
;Recieves :    EAX 		=>           LENGTH OF  Encrypted_Message
;    
;--------------------------------------------------------------------------------------------------------------------------------
Encryption_Decryption PROC USES EAX 

MOV EDX,0
MOV EBX ,2
DIV EBX

MOV ECX ,EAX                                       
Enc_Or_Dec :
      
	  CALL Get_Index_OF_Letters 
      MOV First_Letter,EBX
	  MOV Secound_Letter,EAX


      CALL  GetRows_OF_Letters
	 
    MOV EAX , First_Letter 
    CMP EAX , Secound_Letter                          ;2- Check Same Row OR Same Column OR Not Same Row&Column
	JB Exchange
    SUB EAX , Secound_Letter
    JMP Check_Position

Exchange :
         MOV EAX , Secound_Letter                
		 SUB EAX , First_Letter

Check_Position : 
               MOV EDX ,First_ROW_NUMBER
			   CMP EDX,Secound_ROW_NUMBER
			   JE Same_Row
               MOV EDX ,0 
			   MOV EBX ,5
			   DIV EBX 
			   CMP EDX ,0
			   JE Same_Column
			   JMP Not_Same
Same_Row : 
MOV EBX , 1
JMP NEXT
Same_Column :
MOV EBX , 2
JMP NEXT
Not_Same : 
MOV EBX , 3

NEXT : 
   CMP Encrypt_Decrypt_Check ,1
   JE Encrypt
   CALL Generate_Decrypted_Message
   JMP CON 
   Encrypt: 
  CALL Generate_Encrypted_Message

  CON :
LOOP Enc_Or_Dec 

RET
Encryption_Decryption ENDP

;------------------------------------------------------------------------------------------------
;Get Index Of Letters In Message To Be Encrypted 
;Recieves :ESI Contains OFFSET of  Message
;          EDI Contains OFFSET of  Encrypted_Message
;Returns  :EBX Contains Index Of First Letter
;          EAX Contains Index Of Secound Letter
;-------------------------------------------------------------------------------------------------
Get_Index_OF_Letters PROC USES ECX EDI EDX
       INC EAX
       MOV ECX,2  

Index :    
              PUSH ECX 

			  MOV EDI ,OFFSET matrix_key
              MOV ECX, lengthof matrix_key
              MOV AL ,[ESI]
			  REPNE SCASB
			  JNE NOT_FOUND
              MOV EAX, LengthOf matrix_key - 1
              SUB EAX, ECX

			  POP ECX
			  CMP ECX,1
			  JE Break     
			  JMP  Next
 NOT_FOUND :
   CMP Ignor_I ,1 
   MOV EDX,74
	   JE J_I  
   CMP Ignor_J,1 
   MOV EDX,73
      JE  J_I 

J_I :
             MOV EDI ,OFFSET matrix_key
              MOV ECX, lengthof matrix_key
              MOV AL ,DL
			  REPNE SCASB
			  JNE NOT_FOUND
              MOV EAX, LengthOf matrix_key - 1
              SUB EAX, ECX
			  POP ECX
			  CMP ECX,1
			  JE Break     
			  JMP  Next
Next: 
      MOV First_Letter ,EAX
      ADD ESI,TYPE Message



  LOOP  Index

Break :
 MOV Secound_Letter ,EAX 
 ADD ESI ,TYPE Message

MOV EBX ,First_Letter

RET
Get_Index_OF_Letters ENDP
;------------------------------------------------------------------------------------------------------------
;Get Number Of Row Of Two Letters 
;Recieves : EBX Contains Index Of First Letter
;           EAX Contains Index Of Secound Letter
;Returns  : EBX Contains Number Of Row Of Secound Letter
;           EAX Conatins Number Of Row Of First   Letter 
;------------------------------------------------------------------------------------------------------------
GetRows_OF_Letters PROC USES  ECX 

 MOV EBX ,First_Letter
 MOV ECX ,2 
GetRows :
         CMP EBX,4
         JBE L1 
         CMP EBX,9
         JBE L2 
         CMP EBX,14
         JBE L3
         CMP EBX,19
         JBE L4
         CMP EBX,24
         JBE L5
		 JMP Conti
 L1 :
 MOV EBX ,0
 JMP Conti
 L2 :
 MOV EBX ,1
 JMP Conti
 L3 :
 MOV EBX ,2
 JMP Conti
 L4 :
 MOV EBX ,3
 JMP Conti
 L5 :
 MOV EBX ,4

 Conti :
                  CMP ECX ,2
				  JE  FIRST
				  JMP SECOUND
FIRST :
   MOV First_ROW_NUMBER ,EBX
   MOV EBX ,Secound_Letter
   JMP NEXT 
SECOUND :
 MOV Secound_ROW_NUMBER ,EBX
 NEXT :

 LOOP GetRows

 MOV EAX ,First_ROW_NUMBER

RET
GetRows_OF_Letters ENDP
;--------------------------------------------------------------------------------------------------------------
;Generate Encrypted Message By Fill NewString
;Recieves : EDI Contains OFFSET Of Encrypted_Message
;           EBX Contains If SameROW OR SameColumn OR NotSame 
;--------------------------------------------------------------------------------------------------------------
Generate_Encrypted_Message PROC USES ECX EAX ESI EBX 


CMP EBX ,1
JE ROW 
CMP EBX ,2 
JE COLUMN 
JMP Not_R_C

ROW :

MOV ECX ,2 
R1 : 

MOV ESI , OFFSET matrix_key
CMP ECX,2 
JE  Row_1
JMP Row_2


Row_1 :

MOV EAX ,First_Letter
JMP CheckLast_Row

Row_2 : 
MOV EAX ,Secound_Letter
JMP CheckLast_Row


CheckLast_Row :
CMP EAX , 4
JE LAST_R 
CMP EAX,9
JE LAST_R 
CMP EAX,14
JE LAST_R
CMP EAX,19
JE LAST_R 
CMP EAX,24
JE LAST_R
JMP NOt_Last_R 
LAST_R :
SUB EAX,4
JMP Fill_Row

NOt_Last_R : 
INC EAX
JMP Fill_Row


Fill_Row: 
MOV EBX ,EAX
ADD ESI ,EBX
MOV EDX ,[ESI]
MOV [EDI],EDX
ADD EDI , TYPE Encrypted_Message

LOOP R1 
JMP Next 

COLUMN :


MOV ECX ,2 
CL1 : 

MOV ESI , OFFSET matrix_key
CMP ECX,2 
JE  COL_1
JMP COL_2

COL_1 :
MOV EAX ,First_Letter
JMP CheckLast_Column 

COL_2 : 
MOV EAX ,Secound_Letter
JMP CheckLast_Column 

CheckLast_Column:
CMP EAX , 20
JE LAST_col 
CMP EAX,21
JE LAST_col 
CMP EAX,22
JE LAST_col
CMP EAX,23
JE LAST_col 
CMP EAX,24
JE LAST_col
JMP NOt_Last_col 

LAST_col :
SUB EAX,20
JMP Fill_Column

NOt_Last_col : 
ADD EAX ,5
JMP Fill_Column



Fill_Column : 
MOV EBX ,EAX
ADD ESI ,EBX
MOV EDX ,[ESI]
MOV [EDI],EDX
ADD EDI , TYPE Encrypted_Message

LOOP CL1
JMP Next


Not_R_C :
    MOV EBX , First_ROW_NUMBER 
    CMP EBX , Secound_ROW_NUMBER                          
	JB Exchange
	MOV First_Bigger ,1
    SUB EBX , Secound_ROW_NUMBER
    JMP Check_And_Fill

Exchange :
         MOV Secound_Bigger ,1
         MOV EBX , Secound_ROW_NUMBER                
		 SUB EBX , First_ROW_NUMBER


Check_And_Fill :
  
MOV ECX ,2 

Check : 
          PUSH ECX
                  CMP ECX ,2
				  JE  FIRST
				  MOV EDX,Secound_Bigger
				  MOV EAX ,Secound_Letter
				  JMP Up_OR_Down
First : 
MOV EDX,First_Bigger
MOV EAX ,First_Letter

Up_OR_Down :
            MOV ECX ,EBX 
		 L9:
               CMP EDX ,1
			  
			   JE UP

			   ADD EAX ,5
			   JMP BREAK 

UP : 
SUB EAX ,5
BREAK :
LOOP L9
Fill_NotSame : 
     MOV ESI , OFFSET matrix_key
	 ADD ESI ,EAX 
	 POP ECX
	 CMP ECX ,2
	 JE L1 
	 MOV BL ,[ESI]
	 JMP CONT 
L1 :
	MOV AL ,[ESI]
   MOV First_V ,AL
CONT : 
	 LOOP Check
	 MOV AL ,First_V
MOV [EDI],BL
ADD EDI ,TYPE Encrypted_Message
MOV [EDI],AL
ADD EDI ,TYPE Encrypted_Message

Next :
MOV Secound_Bigger ,0
MOV First_Bigger ,0
RET
Generate_Encrypted_Message ENDP


;-----------------------------------------------------------------------------------------------------
; Function: Filtrates the  String by removeing X .
; Resives :ESI = OFFSET of  ,
;EDX = OFFSET of String3 "NEW STRING" ,
;EAX = Lenghth of String2
; Returns : New String "String2" , EBX = Lenghth of String2 , EDX =  OFFSET of String2
;-----------------------------------------------------------------------------------------------------

FiltrateString PROC

	PUSH EDX
	PUSH ESI

	JMP Lable1

	Lable1:
		POP ESI 
	MOV EDI , ESI
	ADD EDI , 2
	MOV ECX , EAX
	MOV EBX , 0
	SUB ECX , 2
	CMP ECX , 2
	JB L8
	Start:
		MOV AL ,BYTE PTR [ESI]
		CMP AL ,BYTE PTR [EDI]
		JE RemoveChar
		JMP Lable2
		RemoveChar:
		    INC ESI
			MOV AL ,BYTE PTR [ESI]
			CMP AL , 'X'
			JE DELETE
			DEC ESI
			JMP Lable2
			DELETE:
			DEC LEN
			DEC EAX
			DEC ECX
			DEC ESI
			MOV AL ,BYTE PTR [ESI]
			MOV [EDX] , AL
			INC EBX
			INC EDX
			ADD ESI , 2
			ADD EDI , 2
			JMP Lable2 
		Lable2:
			MOV AL ,BYTE PTR [ESI]
			MOV [EDX] , AL
			INC EBX
			INC EDX
			INC ESI
			INC EDI
	LOOP Start
	MOV EAX,LEN
	L8:
	MOV ECX , 2
	L5:
	MOV BL , BYTE PTR [ESI]
	CMP BL ,'X'
	je L6
	MOV BL ,BYTE PTR [ESI]
	CMP BL ,'Z'
	je L6
	MOV AL ,BYTE PTR [ESI]
	CMP AL,10d
	JNE L
	JMP E
	L: MOV [EDX],AL
	INC EBX
	INC ESI
	INC EDX
	JMP N
	L6:
	
		DEC EAX 
		MOV BL , ' '
		MOV [ESI] , BL
		N:
	LOOP L5 
	E:
	
	POP EDX
	RET
FiltrateString ENDP
;-----------------------------------------------------------------------------------------------------------------------------------
; Decryption : USE Playfair Ciphering   TO DECRYPT  MESSAGE
;  Receives    :    ESI => PATH OF FILE  
;                          ECX:  LEN  OF FILE 
;  RETURN : PATH OF OUT FILE
;--------------------------------------------------------------------------------------------------------------------------------
Decryption PROC	 filename_U: ptr byte	,size_file :Dword  , OutputC: ptr byte
pushad
pushfd
call clear_all
mov ecx,   size_file
mov FileSize ,ecx
cld
mov esi,filename_U
mov edi, offset fileName
rep movsb

MOV Encrypt_Decrypt_Check,2
l7:
mov ebx,0


call ReadFromF


MOV ECX,  message_AllowedLength
MOV ESI, offset Message
;----------------------------------
CALL CapitalAllLetters

MOV ECX, Message_Length
CALL PrepareMessage


MOV ESI,OFFSET preparedString                 
MOV EDI ,OFFSET UpdatedMsg
mov ECX, preparedString_length            ;The Real length 
CALL RemoveSpaces       ;If there is spaces between the words this function will remove it

MOV LEN,EBX

 
MOV ecx, LEN_Key 
MOV ESI ,OFFSET Key 
CALL CapitalAllLetters
mov ecx,LEN_Key
CALL Gen_Key_ToMatrix
mov eax,LEN_Key
CALL Complete_matrix
	   MOV ESI ,OFFSET UpdatedMsg
       MOV EDI ,OFFSET Decrypted_Message
	   MOV EAX,LEN
	   MOV Encrypt_Decrypt_Check ,2
	   CALL Encryption_Decryption
	   MOV ESI , OFFSET Decrypted_Message
       MOV EDX , OFFSET MessageAferDec
       call FiltrateString	  

	   WriteF	:
	   CALL SaveInFileProcDe	 
		 cld
       mov ecx,size_file
       add ecx,10
       mov edi,  OutputC
       mov esi,offset OutputFileName
       rep movsb
popfd
popad
ret 
Decryption ENDP
;--------------------------------------------------------------------------------------------------------------
;Generate Decrypted Message By Fill NewString
;Recieves : EDI Contains OFFSET Of DecryptedMessage
;           EBX Contains If SameROW OR SameColumn OR NotSame 
;--------------------------------------------------------------------------------------------------------------
Generate_Decrypted_Message PROC USES ECX EAX ESI EBX 
CMP EBX ,1
JE ROW 
CMP EBX ,2 
JE COLUMN 
JMP Not_R_C

ROW :

MOV ECX ,2 
R1 : 

MOV ESI , OFFSET matrix_key
CMP ECX,2 
JE  Row_1
JMP Row_2


Row_1 :

MOV EAX ,First_Letter
JMP CheckLast_Row

Row_2 : 
MOV EAX ,Secound_Letter
JMP CheckLast_Row


CheckLast_Row :
CMP EAX ,0
JE LAST_R 
CMP EAX,5
JE LAST_R 
CMP EAX,10
JE LAST_R
CMP EAX,15
JE LAST_R 
CMP EAX,20
JE LAST_R
JMP NOt_Last_R 
LAST_R :
ADD EAX,4
JMP Fill_Row

NOt_Last_R : 
DEC EAX
JMP Fill_Row


Fill_Row: 
MOV EBX ,EAX
ADD ESI ,EBX
MOV EDX ,[ESI]
MOV [EDI],EDX
ADD EDI , TYPE MessageAferDec

LOOP R1 
JMP Next 

COLUMN :


MOV ECX ,2 
CL1 : 

MOV ESI , OFFSET matrix_key
CMP ECX,2 
JE  COL_1
JMP COL_2

COL_1 :
MOV EAX ,First_Letter
JMP CheckLast_Column 

COL_2 : 
MOV EAX ,Secound_Letter
JMP CheckLast_Column 

CheckLast_Column:
CMP EAX , 0
JE LAST_col 
CMP EAX,1
JE LAST_col 
CMP EAX,2
JE LAST_col
CMP EAX,3
JE LAST_col 
CMP EAX,4
JE LAST_col
JMP NOt_Last_col 

LAST_col :
ADD EAX,20
JMP Fill_Column

NOt_Last_col : 
SUB EAX ,5
JMP Fill_Column



Fill_Column : 
MOV EBX ,EAX
ADD ESI ,EBX
MOV EDX ,[ESI]
MOV [EDI],EDX
ADD EDI , TYPE Encrypted_Message

LOOP CL1
JMP Next

Not_R_C :
    MOV EBX , First_ROW_NUMBER 
    CMP EBX , Secound_ROW_NUMBER                          
	JB Exchange
	MOV First_Bigger ,1
    SUB EBX , Secound_ROW_NUMBER
    JMP Check_And_Fill

Exchange :
         MOV Secound_Bigger ,1
         MOV EBX , Secound_ROW_NUMBER                
		 SUB EBX , First_ROW_NUMBER


Check_And_Fill :
  
MOV ECX ,2 

Check : 
          PUSH ECX
                  CMP ECX ,2
				  JE  FIRST
				  MOV EDX,Secound_Bigger
				  MOV EAX ,Secound_Letter
				  JMP Up_OR_Down
First : 
MOV EDX,First_Bigger
MOV EAX ,First_Letter

Up_OR_Down :
            MOV ECX ,EBX 
		 L9:
               CMP EDX ,1
			  
			   JE UP

			   ADD EAX ,5
			   JMP BREAK 

UP : 
SUB EAX ,5
BREAK :
LOOP L9
Fill_NotSame : 
     MOV ESI , OFFSET matrix_key
	 ADD ESI ,EAX 
	 POP ECX
	 CMP ECX ,2
	 JE L1 
	 MOV BL ,[ESI]
	 JMP CONT 
L1 :
	MOV AL ,[ESI]
   MOV First_V ,AL
CONT : 
	 LOOP Check
	 MOV AL ,First_V
MOV [EDI],BL
ADD EDI ,TYPE Encrypted_Message
MOV [EDI],AL
ADD EDI ,TYPE Encrypted_Message

Next :
MOV Secound_Bigger ,0
MOV First_Bigger ,0
RET
Generate_Decrypted_Message ENDP
;-----------------------------------------------------------------------------------------------------------------
;-----------------------------------------------------------------------------------------------------------------
CLEAR_all PROC
MOV Input_Char , 0
MOV Counter_Length ,0
MOV Counter , 0
MOV UpdatedMsg   , ' '
MOV Ignor_J   , 0
MOV Ignor_I      , 0
MOV Encrypted_Message    , ' '
MOV Decrypted_Message   , ' '
MOV First_Letter  , 0
MOV Secound_Letter       ,   0
MOV EDX,First_ROW_NUMBER
XOR First_ROW_NUMBER    ,EDX
MOV EDX ,			  Secound_ROW_NUMBER
xor Secound_ROW_NUMBER     ,EDX
mov edx,	  First_Bigger
MOV First_Bigger   	 ,edx
MOV Secound_Bigger , 0

MOV First_V  ,0
MOV Encrypt_Decrypt_Check , 0
MOV LEN , 0
MOV MessageAferDec 	 ,0

MOV Message ,0
MOV message_AllowedLength , 10000
MOV message_Length , 0
MOV preparedString ,0
MOV preparedString_length , 0
MOV count_arr_length , 0

MOV Key , 0
MOV matrix_key , 0
 ;char_alp   'A' , 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'
  ;                 '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0','0'
MOV ECX ,25
MOV EBX,25
CLEAR_ALPH:
MOV char_alp[EBX] , '0' 
INC EBX
LOOP CLEAR_ALPH
  ret
CLEAR_all ENDP
END DLLMAIN