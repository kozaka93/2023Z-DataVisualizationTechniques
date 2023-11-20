package pl.edu.pw.mini.zpoif.task4a.szybowiec.pdt;

import java.time.LocalTime;

public class WpisPelny extends Wpis {

	protected LocalTime godzinaStartu;
	protected LocalTime godzinaLadawania;
	protected String zadanie;

	public WpisPelny(String imie, String nazwisko, int czasLotu, LocalTime godzinaStartu, LocalTime godzinaLadawania,
			String zadanie) {
		super(imie, nazwisko, czasLotu);
		this.godzinaStartu = godzinaStartu;
		this.godzinaLadawania = godzinaLadawania;
		this.zadanie = zadanie;
	}

}
