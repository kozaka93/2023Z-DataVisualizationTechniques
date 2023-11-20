package pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina;

import pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina.inne.Instruktor;
import pl.edu.pw.mini.zpoif.task4a.szybowiec.pdt.PokladowyDziennikTechniczny;

public class KabinaInstruktora extends Kabina {

	protected PokladowyDziennikTechniczny pokladowyDziennikTechniczny;

	public KabinaInstruktora(String id, PokladowyDziennikTechniczny pokladowyDziennikTechniczny) {
		super(id);
		this.pokladowyDziennikTechniczny = pokladowyDziennikTechniczny;
		pilot = new Instruktor();
	}

}
