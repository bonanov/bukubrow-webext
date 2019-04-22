import React from 'react';
import { NonEmptyList } from 'purify-ts/NonEmptyList';
import { connect } from 'react-redux';
import { scrollToTop } from 'Modules/scroll-window';
import { AppState } from 'Store';
import { setSearchFilterWithResets, syncBookmarks } from 'Store/epics';
import { setAddBookmarkModalDisplay } from 'Store/bookmarks/actions';
import { getUnlimitedFilteredBookmarks } from 'Store/selectors';
import SearchControls from './search-controls';
import { setDisplayOpenAllBookmarksConfirmation, setDisplayTutorialMessage, setPage } from 'Store/user/actions';
import { Page } from 'Store/user/types';

type StateProps = ReturnType<typeof mapStateToProps>;
type DispatchProps = UnwrapThunkActions<typeof mapDispatchToProps>;

type Props = StateProps & DispatchProps;

const SearchControlsContainer: Comp<Props> = props => (
	<SearchControls
		onStagedBookmarks={props.openStagedGroups}
		onAdd={props.openAddModal}
		updateTextFilter={(text) => { props.setSearchFilter(text); scrollToTop(); }}
		openAllVisibleBookmarks={props.openAllFilteredBookmarks}
		refreshBookmarks={() => {
			props.syncBookmarks();
			props.setDisplayTutorialMessage(false);
		}}
		textFilter={props.searchFilter}
		shouldEnableSearch={props.searchEnabled}
		shouldEnableOpenStaged={!!props.numStagedItems}
		shouldEnableOpenAll={!!props.numFilteredBookmarks}
		shouldEnableAddBookmark={props.canAddBookmarks}
		numMatches={props.numFilteredBookmarks}
	/>
);

const mapStateToProps = (state: AppState) => ({
	searchFilter: state.input.searchFilter,
	searchEnabled: NonEmptyList.isNonEmpty(state.bookmarks.bookmarks),
	canAddBookmarks: !state.user.displayTutorialMessage,
	numFilteredBookmarks: getUnlimitedFilteredBookmarks(state).length,
	numStagedItems: state.bookmarks.stagedBookmarksGroups.length,
	displayAdd: state.bookmarks.displayAddBookmarkModal,
});

const mapDispatchToProps = {
	syncBookmarks,
	setDisplayTutorialMessage,
	setSearchFilter: setSearchFilterWithResets,
	openStagedGroups: setPage.bind(null, Page.StagedGroupsList),
	openAddModal: setAddBookmarkModalDisplay.bind(null, true),
	openAllFilteredBookmarks: setDisplayOpenAllBookmarksConfirmation.bind(null, true),
};

export default connect(mapStateToProps, mapDispatchToProps)(SearchControlsContainer);
