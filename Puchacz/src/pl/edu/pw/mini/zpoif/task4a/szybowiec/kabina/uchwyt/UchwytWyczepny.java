package pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina.uchwyt;

import pl.edu.pw.mini.zpoif.task4a.szybowiec.mechanizm.MechanizmWyczepiajacy;

public class UchwytWyczepny extends Uchwyt {
	
	protected MechanizmWyczepiajacy mechanizmWyczepiajacy;
	
	public UchwytWyczepny() {
		super(Kolor.ZOLTY);
		mechanizmWyczepiajacy = new MechanizmWyczepiajacy();
	}

	@Override
	public void pociagnij(int wartosc) {
		mechanizmWyczepiajacy.wybierz(wartosc);
	}

	@Override
	public void popchnij(int wartosc) {
		mechanizmWyczepiajacy.oddaj(wartosc);
	}

}
