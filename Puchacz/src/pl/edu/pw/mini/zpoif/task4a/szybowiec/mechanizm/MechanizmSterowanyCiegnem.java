package pl.edu.pw.mini.zpoif.task4a.szybowiec.mechanizm;

public abstract class MechanizmSterowanyCiegnem {
	
	private int polozenieUchwytu;

	public void wybierz(int zmianaWybrania) {
		polozenieUchwytu += zmianaWybrania;
	}

	public void oddaj(int zmianaOddania) {
		polozenieUchwytu -= zmianaOddania;
	}

	public int getPolozenieUchwytu() {
		return polozenieUchwytu;
	}

}
