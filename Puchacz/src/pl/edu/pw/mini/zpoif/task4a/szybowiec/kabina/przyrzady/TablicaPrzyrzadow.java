package pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina.przyrzady;

public class TablicaPrzyrzadow {

	private static final String TYP = "SZD-50-3";
	private String napis = "Pamietaj o checkliscie przed startem!";
	private Wysokosciomierz wysokosciomierz;
	private Wariometr wariometr;
	private String id = "";

	public TablicaPrzyrzadow(String id) {
		wysokosciomierz = new Wysokosciomierz();
		wariometr = new Wariometr();
		this.id = id;
	}

}
