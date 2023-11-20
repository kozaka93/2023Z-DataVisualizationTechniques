package pl.edu.pw.mini.zpoif.task4a.szybowiec;

import pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina.KabinaInstruktora;
import pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina.KabinaUcznia;
import pl.edu.pw.mini.zpoif.task4a.szybowiec.pdt.PokladowyDziennikTechniczny;

public class Puchacz extends Szybowiec {

	private KabinaUcznia kabinaPierwsza;
	private KabinaInstruktora kabinaDruga;

	public Puchacz(String id, PokladowyDziennikTechniczny pokladowyDziennikTechniczny) {
		this.kabinaPierwsza = new KabinaUcznia(id);
		this.kabinaDruga = new KabinaInstruktora(id, pokladowyDziennikTechniczny);
	}

}
