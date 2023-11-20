package pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina;

import pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina.inne.Pilot;
import pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina.przyrzady.TablicaPrzyrzadow;
import pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina.uchwyt.UchwytHamulcaAerodynamicznego;
import pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina.uchwyt.UchwytHamulcaKola;
import pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina.uchwyt.UchwytTrymera;
import pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina.uchwyt.UchwytWyczepny;

public abstract class Kabina {

	private TabliczkaInformacyjna tabliczkaInformacyjna;
	protected TablicaPrzyrzadow tablicaPrzyrzadow;
	protected Pilot pilot;

	protected UchwytHamulcaAerodynamicznego uchwytHamulcaAerodynamicznego;
	protected UchwytHamulcaKola uchwytHamulcaKola;
	protected UchwytWyczepny uchwytWyczepny;
	private UchwytTrymera uchwytTrymera;
	
	protected Kabina(String id) {
		tabliczkaInformacyjna = new TabliczkaInformacyjna();
		tablicaPrzyrzadow = new TablicaPrzyrzadow(id);
		uchwytHamulcaAerodynamicznego = new UchwytHamulcaAerodynamicznego();
		uchwytHamulcaKola = new UchwytHamulcaKola();
		uchwytTrymera = new UchwytTrymera();
		uchwytWyczepny = new UchwytWyczepny();
	}
	
}
