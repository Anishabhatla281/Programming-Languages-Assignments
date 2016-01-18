import java.util.*;
	
	interface Sorted<T extends Comparable<T>> extends List<T>{		
		@SuppressWarnings("hiding")
		<T extends Comparable<T>> Sorted<T> merge(Sorted<T> s);
	}
	
	class SortedList<T extends Comparable<T>> extends ArrayList<T> implements Sorted<T>, Comparable<T>{
		
		private static final long serialVersionUID = 1L;

		public boolean add(T e) {
			for(int i=0;i<this.size();i++){
				if(e.compareTo(this.get(i))==-1)
				{ 
					this.add(i, e);
					return true;
				}
			}
			this.add(this.size(), e);
			return true;			
		}
		
		int compareTo(SortedList<T> o){
			int i=0;
			int c=0;
			for(i=0;(i<this.size()&&i<o.size());i++){
				c = this.get(i).compareTo(o.get(i));
				if(c==0){
				}
				else if(c==-1)
					return -1;				
				else if(c==1)
					return 1;
			}
				if(i<this.size()){
					return 1;
				}	
				else if(i<o.size()){
					return -1;
				}
				else
					return 0;
		}
		
		@SuppressWarnings({ "hiding", "unchecked" })
		public <T extends Comparable<T>> Sorted<T> merge(Sorted<T> s){
			for (int i = 0, j= 0; j< this.size(); i++){
				T e = (T) this.get(j);
				if(i==s.size() || ((s.get(i).compareTo(e))==1)){
					s.add(i, e);
					j++;
				}						
			}
			return s;
		}
		@Override
		public String toString() {
			String s="";
			for(int i=0;i<this.size();i++){
				s+=this.get(i)+" "; 
			}
			return "[[" +s +"]]";
		    }

		@Override
		public int compareTo(T o) {
			return 0;
		}
		}
	
	class A implements Comparable<A>{
		
		int x;
		int y;
		
		A(Integer x, Integer y){
			this.x=x;
			this.y=y;
		}
		
		public int findSum(){
			return x+y;
		}
			
		public int compareTo(A a2){
			if((this.findSum())<(a2.findSum()))
			{	
				return -1;
			}
			else if((this.findSum())>(a2.findSum()))
			{
				return 1;
			}
			else
				return 0;
		}
		
		public String toString() {
			return "A<"+x +","+y+">";
		    }
	}
	
	class B extends A{
		
		int z;
		B(Integer x, Integer y){
			super(x,y);
		}
		
		B(Integer x, Integer y, Integer z){
			super(x,y);
			this.z=z;
		}
		
		@Override
		public int findSum(){
			return x+y+z;
		}
		@Override
		public int compareTo(A b2){
			if((this.findSum())<(b2.findSum()))
			{
				return -1;
			}
			else if((this.findSum())>(b2.findSum()))
			{
				return 1;
			}
			else
				return 0;
		}
		
		public String toString() {
			return "B<"+x +","+y+","+z+">";
		    }
	}
	
	class Part1{
		public static void main(String[] args){
		test();
		}
		static <T extends Comparable<T>> void addToSortedList(SortedList<T> L, T z){
			L.add(z);
		}
		static void test(){
			SortedList<A> c1 = new SortedList<A>();
			SortedList<A> c2 = new SortedList<A>();
			for(int i = 35; i >= 0; i-=5) {
				addToSortedList(c1, new A(i,i+1));
				addToSortedList(c2, new B(i+2,i+3,i+4));
			}
			
			System.out.print("c1: ");
			System.out.println(c1);
			System.out.print("c2: ");
			System.out.println(c2);
			
			switch (c1.compareTo(c2)) {
			case -1: 
			    System.out.println("c1 < c2");
			    break;
			case 0:
			    System.out.println("c1 = c2");
			    break;
			case 1:
			    System.out.println("c1 > c2");
			    break;
			default:
			    System.out.println("Uh Oh");
			    break;
			}
			Sorted<A> res = c1.merge(c2);
			System.out.print("Result: ");
			System.out.println(res);
			
			}		
	}
	

