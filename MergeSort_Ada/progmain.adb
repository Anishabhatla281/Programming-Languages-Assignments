with text_io; use text_io; 
with ada.integer_text_io; use ada.integer_text_io; 
with Sort; use Sort;

procedure ProgMain is
		
		A : SortedArray;
    	TempArray: SortedArray;
    	S : Integer;
        Integer_File : FILE_TYPE;
        Index : Integer;

        procedure ReadProc is

        task type Reader;

        Read_Object : Reader;

        task body Reader is
    	begin
    			Open(Integer_File, In_File, "input.txt");            -- Assuming that the name of the file is input
                while not End_Of_File(Integer_File) loop
                for i in 1..SIZE loop
                if End_Of_Line(Integer_File) then
                new_Line;
                skip_Line(Integer_File);
                else
                Get(Integer_File, Index);
                if(Index <= 500 and Index >= -500) then
                A(i) := Index;
                end if;
                end if;
                end loop;
                end loop;
                Close(Integer_File);
        end Reader;

        begin
        null;
        end ReadProc;

        procedure SumAndPrint is

        task type Sum;
        task type Printer is
        entry Start(S : in Integer);
        end Printer;

        Sum_Object : Sum;
        Printer_Object : Printer;   

    	task body Sum is
    	begin
    			S := 0;
    			for i in 1 .. SIZE loop
    			S := S + A(i);
    			end loop;
                Printer_Object.Start(S);
        end Sum;

    	task body Printer is
    	begin
    		for i in 1 .. SIZE loop
    		put(A(i));
            new_line;
    		end loop;
            accept Start(S: in Integer) do
            put("The sum of the elements of the array is:");
            put(S);
            end Start;
    	end Printer;
    
        begin
        null;
        end SumAndPrint;

   	begin
    	ReadProc;
        TempArray := A;
        MergeSort(A, TempArray, 1, SIZE);
        put("Elements of the sorted array are:");
        new_line;
    	SumAndPrint;
    end ProgMain;