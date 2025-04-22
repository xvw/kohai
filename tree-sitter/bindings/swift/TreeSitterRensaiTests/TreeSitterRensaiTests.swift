import XCTest
import SwiftTreeSitter
import TreeSitterRensai

final class TreeSitterRensaiTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_rensai())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Rensai grammar")
    }
}
