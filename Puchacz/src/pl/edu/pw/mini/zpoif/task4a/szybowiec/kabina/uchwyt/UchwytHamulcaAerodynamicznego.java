package pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina.uchwyt;

import pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina.mechanizm.hamulec.HamulecAerodynamiczny;

public class UchwytHamulcaAerodynamicznego extends Uchwyt {
	
	private HamulecAerodynamiczny hamulecAerodynamiczny;
	
	public UchwytHamulcaAerodynamicznego() {
		super(Kolor.NIEBIESKI);
		hamulecAerodynamiczny = new HamulecAerodynamiczny();
	}

	@Override
	public void pociagnij(int wartosc) {
		hamulecAerodynamiczny.wybierz(wartosc);
	}

	@Override
	public void popchnij(int wartosc) {
		hamulecAerodynamiczny.oddaj(wartosc);
	}

}
