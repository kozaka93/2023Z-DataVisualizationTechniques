package pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina.uchwyt;

import pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina.mechanizm.hamulec.HamulecKola;
import pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina.mechanizm.hamulec.HamulecKolaZWada;

public class UchwytHamulcaKola extends Uchwyt {

	protected HamulecKola hamulecKola;
	
	public UchwytHamulcaKola() {
		super(Kolor.NIEBIESKI);
		hamulecKola = new HamulecKolaZWada();
	}
	@Override
	public void pociagnij(int wartosc) {
		hamulecKola.wybierz(wartosc);
	}
	@Override
	public void popchnij(int wartosc) {
		hamulecKola.oddaj(wartosc);
	}

}
