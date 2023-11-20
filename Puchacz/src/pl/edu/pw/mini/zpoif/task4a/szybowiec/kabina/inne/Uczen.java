package pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina.inne;

public class Uczen extends Pilot {

	private int licznikLotowSamodzielnych;

	public Uczen() {
		prywatneMotto = "Dobrego zeglarza tylko sztormy wychowuja";
	}

	public Uczen(String imie, String nazwisko, Nakolannik nakolannik) {
		super(imie, nazwisko, nakolannik);
	}

	public Uczen(String imie, String nazwisko) {
		super(imie, nazwisko);
	}

}
