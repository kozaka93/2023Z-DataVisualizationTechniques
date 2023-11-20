package pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina.uchwyt;

import pl.edu.pw.mini.zpoif.task4a.szybowiec.mechanizm.Trymer;

public class UchwytTrymera extends Uchwyt {

	protected Trymer trymer;
	
	public UchwytTrymera() {
		super(Kolor.NIEBIESKI);
		trymer = new Trymer();
	}

	@Override
	public void pociagnij(int wartosc) {
		trymer.wybierz(wartosc);
	}

	@Override
	public void popchnij(int wartosc) {
		trymer.oddaj(wartosc);
	}

}
