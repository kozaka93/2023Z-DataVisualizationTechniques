package pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina;

import pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina.inne.Uczen;
import pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina.uchwyt.UchwytWywietrznika;

public class KabinaUcznia extends Kabina {

	protected UchwytWywietrznika uchwytWywietrznika;

	public KabinaUcznia(String id) {
		super(id);
		pilot = new Uczen();
	}
	
}
