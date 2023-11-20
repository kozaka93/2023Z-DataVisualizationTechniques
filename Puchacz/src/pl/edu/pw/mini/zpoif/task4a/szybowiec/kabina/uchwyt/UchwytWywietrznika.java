package pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina.uchwyt;

import pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina.Wywietrznik;

public class UchwytWywietrznika extends Uchwyt {

	protected Wywietrznik wywietrznik;
	
	public UchwytWywietrznika() {
		super(Kolor.BRAZOWY);
	}

	@Override
	public void pociagnij(int wartosc) {
		System.out.println("Pssssss....");
	}

	@Override
	public void popchnij(int wartosc) {
		System.out.println("Fupppp....");
	}

}
