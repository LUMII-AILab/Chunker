/*******************************************************************************
 * Copyright 2008, 2009 Institute of Mathematics and Computer Science, University of Latvia; 
 * Author: Ilmārs Poikāns
 *     This program is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 * 
 *     This program is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 * 
 *     You should have received a copy of the GNU General Public License
 *     along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *******************************************************************************/
package lv.semti.PrologInterface;

import java.util.ArrayList;
import java.util.List;

import lv.semti.morphology.analyzer.*;

public class PrologWrapper {

	private static Analyzer analyzer;
	
	static {
		try {
			analyzer = new Analyzer();
			// log = new PrintWriter(new BufferedWriter(new OutputStreamWriter(new FileOutputStream("prologwrapper.log"), "UTF-8")));
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public static void main(String[] args) throws Exception {

		String word = args.length > 0 ? args[0] : "zaļu";
		System.out.println(analyze(word));
	}
	
	// analyze without guessing words
	
	public static String analyze(String word) {
		return analyze(word, false);
	}
	
	public static String analyze(String word, boolean guess) {
		return analyze(word, guess, guess, guess, guess);
	}

	public static String analyze(String word, boolean guessNouns, boolean guessVerbs, boolean guessParticiples, boolean guessAdjectives) {
		return analyze(word, guessNouns, guessVerbs, guessParticiples, guessAdjectives, true, false, false);
	}	
	
	public static synchronized String analyze(String word, boolean guessNouns, boolean guessVerbs, boolean guessParticiples, boolean guessAdjectives, boolean allowVocative, boolean enableDiminutive, boolean morphoDefaults) {
		
		MarkupConverter.defaulti = morphoDefaults;
		
		analyzer.enableGuessing = (guessNouns || guessVerbs || guessParticiples || guessAdjectives);
		analyzer.guessNouns = guessNouns;
		analyzer.guessVerbs = guessVerbs;
		analyzer.guessParticiples = guessParticiples;
		analyzer.guessAdjectives = guessAdjectives;
		analyzer.enableVocative = allowVocative;
		analyzer.enableDiminutive = enableDiminutive;
		
		String results = toProlog(word);
		
		return results;
	}

	public static List<String> words2tokens(List<Word> words) {
		
		List<String> tokens = new ArrayList<String>(words.size());

		for (Word word : words)
			tokens.add(word.getToken());
		
		return tokens;
	}

	public static synchronized List<Word> tokenize(String sentence) {
		return Splitting.tokenize(analyzer, sentence);
	}
	
	private static String toProlog(String word) {
		return toProlog(word,false);
	}
	
	public static String toProlog(String word, boolean toolgenerated) {
		Word vards = analyzer.analyze(word);		
		return MarkupConverter.wordToChunkerFormat(vards, toolgenerated);
	}

}
