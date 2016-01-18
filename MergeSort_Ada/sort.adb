with text_io; use text_io; 
with ada.integer_text_io; use ada.integer_text_io; 

package body Sort is 
	procedure MergeSort(A, TempArray: in out SortedArray; LowerIndex, UpperIndex: in Integer) is
		Middle : Integer;
		
	procedure Merge(A, TempArray: in out SortedArray; LowerIndex, MiddleIndex, UpperIndex: in Integer) is
		MidIndex : Integer;
		LowIndex : Integer;
		TempLowerIndex: Integer;
		TempMiddleIndex: Integer;
		begin 
		MidIndex := MiddleIndex - 1;
		LowIndex := LowerIndex;
		TempLowerIndex := LowerIndex;
		TempMiddleIndex := MiddleIndex;
        while ((TempLowerIndex <= MidIndex) and (TempMiddleIndex <= UpperIndex)) loop
			if(A(TempLowerIndex) < A(TempMiddleIndex)) then
                TempArray(LowIndex) := A(TempLowerIndex);
                LowIndex := LowIndex + 1;
                TempLowerIndex := TempLowerIndex + 1;
            else
                TempArray(LowIndex) := A(TempMiddleIndex);
                LowIndex := LowIndex + 1;
                TempMiddleIndex := TempMiddleIndex + 1;
            end if;
		end loop;
		while(TempLowerIndex <= MidIndex) loop
            TempArray(LowIndex) := A(TempLowerIndex);
            LowIndex := LowIndex + 1;
            TempLowerIndex := TempLowerIndex + 1;
        end loop;
        while(TempMiddleIndex <= UpperIndex) loop
            TempArray(LowIndex) := A(TempMiddleIndex);
            LowIndex := LowIndex + 1;
            TempMiddleIndex := TempMiddleIndex + 1;
        end loop;
        for i in 1 .. SIZE loop
        	A(i) := TempArray(i);
        end loop;
    end Merge;

    procedure RunTasks is
		task type Task1;
		task type Task2;

        Task1_Object : Task1;
        Task2_Object: Task2;
	
		task body Task1 is
    	begin
                MergeSort(A, TempArray, LowerIndex, Middle);
    	end Task1;

    	task body Task2 is
    	begin
               MergeSort(A, TempArray, Middle+1, UpperIndex);
    	end Task2;
    
    begin
    null;
    end RunTasks;

    begin
		Middle := 0;
        if LowerIndex < UpperIndex then
			Middle := (LowerIndex + UpperIndex)/2;
            RunTasks;
            Merge(A, TempArray, LowerIndex, (Middle+1), UpperIndex);
		end if;
	end MergeSort;

end Sort;