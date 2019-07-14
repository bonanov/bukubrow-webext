import { Option } from 'fp-ts/lib/Option';

export type Bookmark = LocalBookmark;

export interface BookmarksState {
	bookmarks: Bookmark[];
	stagedBookmarksGroups: StagedBookmarksGroup[];
	limitNumRendered: boolean;
	focusedBookmarkIndex: Option<number>;
	bookmarkEditId: Option<Bookmark['id']>;
	bookmarkDeleteId: Option<Bookmark['id']>;
	stagedBookmarksGroupEditId: Option<StagedBookmarksGroup['id']>;
	stagedBookmarksGroupBookmarkEditId: Option<Bookmark['id']>;
	displayDeleteBookmarkModal: boolean;
}

export enum BookmarksActionTypes {
	SetAllBookmarks = 'SET_ALL_BOOKMARKS',
	SetAllStagedBookmarksGroups = 'SET_ALL_STAGED_BOOKMARKS_GROUPS',
	DeleteStagedBookmarksGroup = 'DELETE_STAGED_BOOKMARKS_GROUP',
	SetLimitNumRendered = 'SET_LIMIT_NUM_RENDERED',
	SetFocusedBookmarkIndex = 'SET_FOCUSED_BOOKMARK_INDEX',
	SetBookmarkEditId = 'SET_BOOKMARK_EDIT_ID',
	SetBookmarkDeleteId = 'SET_BOOKMARK_DELETE_ID',
	SetStagedBookmarksGroupEditId = 'SET_STAGED_BOOKMARKS_GROUP_EDIT_ID',
	SetStagedBookmarksGroupBookmarkEditId = 'SET_STAGED_BOOKMARKS_GROUP_BOOKMARK_EDIT_ID',
	UpdateStagedBookmarksGroupBookmark = 'UPDATE_STAGED_BOOKMARKS_GROUP_BOOKMARK',
	DeleteStagedBookmarksGroupBookmark = 'DELETE_STAGED_BOOKMARKS_GROUP_BOOKMARK',
	SetDeleteBookmarkModalDisplay = 'SET_DELETE_BOOKMARK_MODAL_DISPLAY',
}
