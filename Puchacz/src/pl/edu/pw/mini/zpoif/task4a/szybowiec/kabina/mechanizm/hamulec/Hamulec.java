package pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina.mechanizm.hamulec;

import pl.edu.pw.mini.zpoif.task4a.szybowiec.mechanizm.MechanizmSterowanyCiegnem;

public abstract class Hamulec extends MechanizmSterowanyCiegnem {
	
	@Override
	public void wybierz(int procentWybrania) {
		super.wybierz(procentWybrania);
		hamuj(getPolozenieUchwytu());
	}

	@Override
	public void oddaj(int procentOddania) {
		super.oddaj(procentOddania);
		odhamuj(getPolozenieUchwytu());
	}

	protected abstract void hamuj(int procentHamowania);

	protected abstract void odhamuj(int prorocentUstawienia);

}
