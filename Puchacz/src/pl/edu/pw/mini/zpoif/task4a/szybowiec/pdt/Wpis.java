package pl.edu.pw.mini.zpoif.task4a.szybowiec.pdt;

public abstract class Wpis {

	protected String imie;
	protected String nazwisko;
	protected int czasLotu;

	public Wpis(String imie, String nazwisko, int czasLotu) {
		this.imie = imie;
		this.nazwisko = nazwisko;
		this.czasLotu = czasLotu;
	}
	
}
