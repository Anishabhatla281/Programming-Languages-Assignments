package Sort is
	SIZE : constant Integer := 40;
	subtype int is Integer range -500 .. 500;
	type SortedArray is array (1 .. SIZE) of int;
	procedure MergeSort(A, TempArray: in out SortedArray; LowerIndex, UpperIndex: in Integer);
end Sort;

