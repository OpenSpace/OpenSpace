import React, { Component } from 'react';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';
import HomeButton from './../presentational/HomeButton';
import { resetStoryTree } from '../../../../api/Actions';

class HomeButtonContainer extends Component {
  constructor(props) {
    super(props);

    this.goToMenu = this.goToMenu.bind(this);
  }

  goToMenu() {
    this.props.ResetStoryTree(true);
  }

  render() {
    return (
      <HomeButton handleClick={this.goToMenu} />
    );
  }
}

const mapDispatchToProps = dispatch => ({
  ResetStoryTree: (reset) => {
    dispatch(resetStoryTree(reset));
  },
});

HomeButtonContainer = connect(
  null,
  mapDispatchToProps,
)(HomeButtonContainer);


HomeButtonContainer.propTypes = {
  ResetStoryTree: PropTypes.func,
};

HomeButtonContainer.defaultProps = {
  ResetStoryTree: () => {},
};

export default HomeButtonContainer;
